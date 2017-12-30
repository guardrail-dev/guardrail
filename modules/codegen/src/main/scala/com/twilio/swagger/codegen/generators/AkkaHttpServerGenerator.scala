package com.twilio.swagger.codegen
package generators

import _root_.io.swagger.models.HttpMethod
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.swagger.codegen.SwaggerUtil
import com.twilio.swagger.codegen.extract.{ScalaPackage, ServerRawResponse, ScalaTracingLabel}
import com.twilio.swagger.codegen.terms.server._
import scala.collection.JavaConverters._
import scala.meta._

object AkkaHttpServerGenerator {
  object ServerTermInterp extends FunctionK[ServerTerm, Target] {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }
    def apply[T](term: ServerTerm[T]): Target[T] = term match {
      case GetFrameworkImports(tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "getFrameworkImports")(s"getFrameworkImports(${tracing})")
        } yield List(
          q"import akka.http.scaladsl.model._"
        , q"import akka.http.scaladsl.marshalling.{Marshal, Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshaller}"
        , q"import akka.http.scaladsl.server.Directives._"
        , q"import akka.http.scaladsl.server.{Directive, Directive0, Route}"
        , q"import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller, FromEntityUnmarshaller}"
        , q"import akka.util.ByteString"
        , q"import scala.language.implicitConversions"
        )

      case ExtractOperations(paths) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"extractOperations(${paths})")
          routes <- paths.map({ case (pathStr, path) =>
            for {
              _ <- Target.log.info("AkkaHttpServerGenerator", "server", "extractOperations")(s"(${pathStr}, ${path})")
              operationMap <- Target.fromOption(Option(path.getOperationMap), "No operations defined")
            } yield {
              operationMap.asScala.map { case (httpMethod, operation) =>
                ServerRoute(pathStr, httpMethod, operation)
              }
            }
          }).sequenceU.map(_.flatten)
        } yield routes

      case GetClassName(operation) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getClassName(${operation})")

          pkg = ScalaPackage(operation).map(_.split('.').toVector).orElse({
            Option(operation.getTags).map { tags =>
              println(s"Warning: Using `tags` to define package membership is deprecated in favor of the `x-scala-package` vendor extension")
              tags.asScala
            }
          }).map(_.toList)
          opPkg = Option(operation.getOperationId()).map(splitOperationParts).fold(List.empty[String])(_._1)
          finalPkg = pkg.map(_ ++ opPkg).getOrElse(opPkg)
          className <- Target.fromOption(NonEmptyList.fromList(finalPkg), s"Unable to determine className for ${operation}")
        } yield className

      case BuildTracingFields(operation, resourceName, tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"buildTracingFields(${operation}, ${resourceName}, ${tracing})")
          res <- if (tracing) {
            for {
              operationId <- Target.fromOption(Option(operation.getOperationId()).map(splitOperationParts).map(_._2), "Missing operationId")
              label = ScalaTracingLabel(operation).map(Lit.String(_)).getOrElse(Lit.String(s"${resourceName.toList.last}:${operationId}"))
            } yield Some((ScalaParameter.fromParam(param"traceBuilder: TraceBuilder"), q"""trace(${label})"""))
          } else Target.pure(None)
        } yield res

      case GenerateResponseDefinitions(operation, protocolElems) =>
        for {
          operationId <- Target.fromOption(Option(operation.getOperationId()).map(splitOperationParts).map(_._2), "Missing operationId")
          responses <- Target.fromOption(Option(operation.getResponses).map(_.asScala), s"No responses defined for ${operationId}")
          responseSuperType = Type.Name(s"${operationId}Response")
          responseSuperTerm = Term.Name(s"${operationId}Response")

          instances <- responses.foldLeft[List[Target[(Defn, Defn, Case)]]](List.empty)({ case (acc, (key, resp)) =>
            acc :+ (for {
              httpCode <- Target.fromOption(HttpHelper(key), s"Unknown HTTP type: ${key}")
              (code, friendlyName) = httpCode
              statusCodeName = Term.Name(friendlyName)
              statusCode = q"StatusCodes.${statusCodeName}"
              valueType <- Option(resp.getSchema).map({ prop =>
                for {
                  meta <- SwaggerUtil.propMeta(prop)
                  resolved <- SwaggerUtil.ResolvedType.resolve(meta, protocolElems)
                  SwaggerUtil.Resolved(baseType, _, baseDefaultValue) = resolved
                } yield baseType
              }).sequenceU
              responseTerm = Term.Name(s"${operationId}Response${statusCodeName.value}")
              responseName = Type.Name(s"${operationId}Response${statusCodeName.value}")
            } yield {
              valueType.fold[(Defn, Defn, Case)](
                ( q"case object ${responseTerm}                      extends ${responseSuperType}(${statusCode})"
                , q"def ${statusCodeName}: ${responseSuperType} = ${responseTerm}"
                , p"case r: ${responseTerm}.type => scala.concurrent.Future.successful(Marshalling.Opaque(() => HttpResponse(r.statusCode)) :: Nil)"
                )
              ) { valueType =>
                ( q"case class  ${responseName}(value: ${valueType}) extends ${responseSuperType}(${statusCode})"
                , q"def ${statusCodeName}(value: ${valueType}): ${responseSuperType} = ${responseTerm}(value)"
                , p"case r@${responseTerm}(value) => Marshal(value).to[ResponseEntity].map { entity => Marshalling.Opaque(() => HttpResponse(r.statusCode, entity=entity)) :: Nil }"
                )
              }
            })
          }).sequenceU

          (terms, aliases, marshallers) = instances.unzip3

          convenienceConstructors = aliases.flatMap({
            case q"def $name(value: $tpe): $_ = $_" => tpe.map { (_, name) }
            case _ => None
          })

          implicitHelpers = convenienceConstructors.groupBy(_._1).flatMap({
            case (tpe, (_, name) :: Nil) => Some(q"implicit def ${Term.Name(s"${name.value}Ev")}(value: ${tpe}): ${responseSuperType} = ${name}(value)")
            case _ => None
          }).toList

          companion = q"""
            object ${responseSuperTerm} {
              implicit val ${Pat.Var(Term.Name(s"${operationId}TRM"))}: ToResponseMarshaller[${responseSuperType}] = Marshaller { implicit ec => resp => ${Term.Name(s"${operationId}TR")}(resp) }
              implicit def ${Term.Name(s"${operationId}TR")}(value: ${responseSuperType})(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] =
                ${Term.Match(Term.Name("value"), marshallers)}

              def apply[T](value: T)(implicit ev: T => ${responseSuperType}): ${responseSuperType} = ev(value)

              ..${implicitHelpers}

              ..${aliases}
            }
          """

        } yield List[Defn](
          q"sealed abstract class ${responseSuperType}(val statusCode: StatusCode)"
        ) ++ terms ++ List[Defn](
          companion
        )

      case GenerateRoute(resourceName, basePath, route@ServerRoute(path, method, operation), tracingFields, responseDefinitions, protocolElems) =>
        // Generate the pair of the Handler method and the actual call to `complete(...)`
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"generateRoute(${resourceName}, ${basePath}, ${route}, ${tracingFields})")
          parameters <- Option(operation.getParameters).map(_.asScala.toList).map(ScalaParameter.fromParameters(protocolElems)).getOrElse(Target.pure(List.empty[ScalaParameter]))
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server", "generateRoute")("Parameters:")
          _ <- parameters.map(parameter => Target.log.debug("AkkaHttpServerGenerator", "server", "generateRoute", "parameter")(s"${parameter}")).sequenceU

          filterParamBy = ScalaParameter.filterParams(parameters)
          bodyArgs = filterParamBy("body").headOption
          formArgs = filterParamBy("formData").toList
          headerArgs = filterParamBy("header").toList
          pathArgs = filterParamBy("path").toList
          qsArgs = filterParamBy("query").toList

          akkaMethod <- httpMethodToAkka(method)
          akkaPath <- pathStrToAkka(basePath, path, pathArgs)
          akkaQs <- qsToAkka(qsArgs)
          akkaBody <- bodyToAkka(bodyArgs)
          akkaForm <- formToAkka(formArgs)
          akkaHeaders <- headersToAkka(headerArgs)
          operationId <- Target.fromOption(Option(operation.getOperationId()).map(splitOperationParts).map(_._2), "Missing operationId")
        } yield {
          val (responseCompanionTerm, responseCompanionType) = (Term.Name(s"${operationId}Response"), Type.Name(s"${operationId}Response"))
          val responseType = ServerRawResponse(operation).filter(_ == true).fold[Type](t"${Term.Name(resourceName)}.${responseCompanionType}")(Function.const(t"HttpResponse"))
          val orderedParameters: List[List[ScalaParameter]] = List((pathArgs ++ qsArgs ++ bodyArgs ++ formArgs ++ headerArgs).toList) ++ tracingFields.map(_._1).map(List(_))
          val fullRouteMatcher = List[Option[Term]](Some(akkaMethod), Some(akkaPath), akkaQs, Some(akkaBody), akkaForm, akkaHeaders, tracingFields.map(_._2)).flatten.reduceLeft { (a, n) => q"${a} & ${n}" }
          val handlerCallArgs: List[List[Term.Name]] = List(List(responseCompanionTerm)) ++ orderedParameters.map(_.map(_.paramName))
          val fullRoute: Term.Apply = orderedParameters match {
            case List(List()) => q"""
              ${fullRouteMatcher} {
                complete(handler.${Term.Name(operationId)}(...${handlerCallArgs}))
              }
              """
            case params =>
              q"""
              ${fullRouteMatcher} { (..${params.flatten.map(p => param"${p.paramName}")}) =>
                complete(handler.${Term.Name(operationId)}(...${handlerCallArgs}))
              }
              """
          }

          val respond: List[List[Term.Param]] = List(List(param"respond: ${Term.Name(resourceName)}.${responseCompanionTerm}.type"))
          val params: List[List[Term.Param]] = respond ++ orderedParameters.map(_.map(_.param))
          RenderedRoute(fullRoute,
            q"""
              def ${Term.Name(operationId)}(...${params}): scala.concurrent.Future[${responseType}]
            """
          , responseDefinitions)
        }

      case CombineRouteTerms(terms) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"combineRouteTerms(<${terms.length} routes>)")
          routes <- Target.fromOption(NonEmptyList.fromList(terms), "Generated no routes, no source to generate")
          _ <- routes.map(route => Target.log.debug("AkkaHttpServerGenerator", "server", "combineRouteTerms")(route.toString)).sequenceU
        } yield routes.tail.foldLeft(routes.head) { case (a, n) => q"${a} ~ ${n}" }

      case RenderHandler(handlerName, methodSigs) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"renderHandler(${handlerName}, ${methodSigs}")
        } yield q"""
          trait ${Type.Name(handlerName)} {
            ..${methodSigs}
          }
        """

      case GetExtraRouteParams(tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getExtraRouteParams(${tracing})")
          res <- if (tracing) {
            Target.pure(List(param"""trace: String => Directive1[TraceBuilder]"""))
          } else Target.pure(List.empty)
        } yield res

      case RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"renderClass(${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams})")
          routesParams = List(param"handler: ${Type.Name(handlerName)}") ++ extraRouteParams
        } yield q"""
          object ${Term.Name(resourceName)} {
            import cats.syntax.either._
            def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap({ req => req.discardEntityBytes().future; Directive.Empty })
            implicit def jsonFSU[T: io.circe.Decoder]: Unmarshaller[String, T] = Unmarshaller[String, T]({
              implicit ev => string =>
               io.circe.Json.fromString(string).as[T]
                .left.flatMap(err => io.circe.jawn.parse(string).flatMap(_.as[T]))
                .fold(scala.concurrent.Future.failed _, scala.concurrent.Future.successful _)
            })
            def routes(..${routesParams})(implicit mat: akka.stream.Materializer): Route = {
              ${combinedRouteTerms}
            }

            ..${responseDefinitions}
          }
        """

      case GetExtraImports(tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getExtraImports(${tracing})")
        } yield if (tracing) List(q"import akka.http.scaladsl.server.Directive1") else List.empty
    }

    def httpMethodToAkka(method: HttpMethod): Target[Term] = method match {
      case HttpMethod.DELETE => Target.pure(q"delete")
      case HttpMethod.GET => Target.pure(q"get")
      case HttpMethod.PATCH => Target.pure(q"patch")
      case HttpMethod.POST => Target.pure(q"post")
      case HttpMethod.PUT => Target.pure(q"put")
      case other => Target.error(s"Unknown method: ${other}")
    }

    def pathSegmentToAkka: ScalaParameter => Target[Term] = { case ScalaParameter(_, param, _, argName, argType) =>
      argType match {
        case t"String" =>         Target.pure(q"Segment")
        case t"Double" =>         Target.pure(q"DoubleNumber")
        case t"BigDecimal" =>     Target.pure(q"Segment.map(BigDecimal.apply _)")
        case t"Int" =>            Target.pure(q"IntNumber")
        case t"Long" =>           Target.pure(q"LongNumber")
        case t"BigInt" =>         Target.pure(q"Segment.map(BigInt.apply _)")
        case tpe@Type.Name(_) =>  Target.pure(q"Segment.flatMap(str => io.circe.Json.fromString(str).as[${tpe}].toOption)")
      }
    }

    def pathStrToAkka(basePath: Option[String], path: String, pathVars: List[ScalaParameter]): Target[Term] = {
      val varRegex = "^\\{([^}]+)\\}$".r
      def getKnownVar(segment: String): Option[Target[Term]] = {
        varRegex.findFirstMatchIn(segment).map { m =>
          val paramName = m.group(1)
          pathVars.find(_.argName.value == paramName)
            .map { param =>
              pathSegmentToAkka(param)
            } getOrElse {
              Target.error(s"Unknown path variable ${paramName} (known: ${pathVars.map(_.argName).mkString(", ")})")
            }
        }
      }

      def addTrailingSlashMatcher(trailingSlash: Boolean, term: Term.Apply): Term =
        if (trailingSlash) q"${term.copy(fun=Term.Name("pathPrefix"))} & pathEndOrSingleSlash"
        else term

      (basePath.getOrElse("") + path).stripPrefix("/") match {
        case "" => Target.pure(q"pathEnd")
        case rest =>
          for {
            segments <- (
              rest.split('/')
                .toList
                .map(_.pure[Option].filterNot(_.isEmpty).fold(Target.error[Term]("Double slashes not supported")) {
                  segment => getKnownVar(segment).getOrElse(Target.pure(Lit.String(segment)))
                }).sequenceU
            )
            pathDirective <- segments match {
              case x :: Nil => Target.pure(q"path(${x})")
              case x :: xs => Target.pure(q"path(${xs.foldLeft(x) { case (a, n) => q"${a} / ${n}" }})")
              case Nil => Target.error("Impossible scenario")
            }
            akkaRoute = addTrailingSlashMatcher(rest.endsWith("/"), pathDirective)
          } yield akkaRoute
      }
    }

    def directivesFromParams(
        required: Term => Type => Target[Term.Apply],
        multi: Term => Type => Target[Term.Apply],
        multiOpt: Term => Type => Target[Term.Apply],
        optional: Term => Type => Target[Term.Apply]
    )(params: List[ScalaParameter]): Target[Option[Term]] = {
      for {
        directives <- params.map({ case ScalaParameter(_, param, _, argName, argType) =>
          param match {
            case param"$_: Option[Iterable[$tpe]]" => multiOpt(Lit.String(argName.value))(tpe)
            case param"$_: Option[Iterable[$tpe]] = $_" => multiOpt(Lit.String(argName.value))(tpe)
            case param"$_: Option[$tpe]" => optional(Lit.String(argName.value))(tpe)
            case param"$_: Option[$tpe] = $_" => optional(Lit.String(argName.value))(tpe)
            case param"$_: Iterable[$tpe]" => multi(Lit.String(argName.value))(tpe)
            case param"$_: Iterable[$tpe] = $_" => multi(Lit.String(argName.value))(tpe)
            case _ => required(Lit.String(argName.value))(argType)
          }
        }).sequenceU
      } yield directives match {
        case Nil => Option.empty
        case x :: xs => Some(xs.foldLeft[Term](x) { case (a, n) => q"${a} & ${n}" })
      }
    }

    def bodyToAkka(body: Option[ScalaParameter]): Target[Term] = {
      Target.pure(
        body.map { case ScalaParameter(_, _, _, _, argType) =>
          q"entity(as[${argType}])"
        } getOrElse {
          q"discardEntity"
        }
      )
    }

    def headersToAkka: List[ScalaParameter] => Target[Option[Term]] = {
      directivesFromParams(
        arg => tpe => Target.pure(q"headerValueByName(${arg})"),
        arg => tpe => Target.error(s"Unsupported Iterable[${arg}]"),
        arg => tpe => Target.error(s"Unsupported Option[Iterable[${arg}]]"),
        arg => tpe => Target.pure(q"optionalHeaderValueByName(${arg})")
      ) _
    }

    def qsToAkka: List[ScalaParameter] => Target[Option[Term]] = {
      directivesFromParams(
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}])"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].*)"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].*).map(Option.apply _)"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].?)")
      ) _
    }

    def formToAkka: List[ScalaParameter] => Target[Option[Term]] = {
      directivesFromParams(
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}])"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].*)"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].*).map(Option.apply _)"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].?)")
      ) _
    }
  }
}
