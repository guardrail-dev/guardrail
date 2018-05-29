package com.twilio.guardrail
package generators

import _root_.io.swagger.models.HttpMethod
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.SwaggerUtil
import com.twilio.guardrail.extract.{ScalaPackage, ScalaTracingLabel, ServerRawResponse}
import com.twilio.guardrail.protocol.terms.server._

import scala.collection.JavaConverters._
import scala.meta._

object AkkaHttpServerGenerator {
  object HttpHelper {
    def apply(code: String): Option[(Int, String)] = {
      code match {
        case "100" => Option((100, "Continue"))
        case "101" => Option((101, "SwitchingProtocols"))
        case "102" => Option((102, "Processing"))

        case "200" => Option((200, "OK"))
        case "201" => Option((201, "Created"))
        case "202" => Option((202, "Accepted"))
        case "203" => Option((203, "NonAuthoritativeInformation"))
        case "204" => Option((204, "NoContent"))
        case "205" => Option((205, "ResetContent"))
        case "206" => Option((206, "PartialContent"))
        case "207" => Option((207, "MultiStatus"))
        case "208" => Option((208, "AlreadyReported"))
        case "226" => Option((226, "IMUsed"))

        case "300" => Option((300, "MultipleChoices"))
        case "301" => Option((301, "MovedPermanently"))
        case "302" => Option((302, "Found"))
        case "303" => Option((303, "SeeOther"))
        case "304" => Option((304, "NotModified"))
        case "305" => Option((305, "UseProxy"))
        case "307" => Option((307, "TemporaryRedirect"))
        case "308" => Option((308, "PermanentRedirect"))

        case "400" => Option((400, "BadRequest"))
        case "401" => Option((401, "Unauthorized"))
        case "402" => Option((402, "PaymentRequired"))
        case "403" => Option((403, "Forbidden"))
        case "404" => Option((404, "NotFound"))
        case "405" => Option((405, "MethodNotAllowed"))
        case "406" => Option((406, "NotAcceptable"))
        case "407" => Option((407, "ProxyAuthenticationRequired"))
        case "408" => Option((408, "RequestTimeout"))
        case "409" => Option((409, "Conflict"))
        case "410" => Option((410, "Gone"))
        case "411" => Option((411, "LengthRequired"))
        case "412" => Option((412, "PreconditionFailed"))
        case "413" => Option((413, "RequestEntityTooLarge"))
        case "414" => Option((414, "RequestUriTooLong"))
        case "415" => Option((415, "UnsupportedMediaType"))
        case "416" => Option((416, "RequestedRangeNotSatisfiable"))
        case "417" => Option((417, "ExpectationFailed"))
        case "418" => Option((418, "ImATeapot"))
        case "420" => Option((420, "EnhanceYourCalm"))
        case "422" => Option((422, "UnprocessableEntity"))
        case "423" => Option((423, "Locked"))
        case "424" => Option((424, "FailedDependency"))
        case "425" => Option((425, "UnorderedCollection"))
        case "426" => Option((426, "UpgradeRequired"))
        case "428" => Option((428, "PreconditionRequired"))
        case "429" => Option((429, "TooManyRequests"))
        case "431" => Option((431, "RequestHeaderFieldsTooLarge"))
        case "449" => Option((449, "RetryWith"))
        case "450" => Option((450, "BlockedByParentalControls"))
        case "451" => Option((451, "UnavailableForLegalReasons"))

        case "500" => Option((500, "InternalServerError"))
        case "501" => Option((501, "NotImplemented"))
        case "502" => Option((502, "BadGateway"))
        case "503" => Option((503, "ServiceUnavailable"))
        case "504" => Option((504, "GatewayTimeout"))
        case "505" => Option((505, "HTTPVersionNotSupported"))
        case "506" => Option((506, "VariantAlsoNegotiates"))
        case "507" => Option((507, "InsufficientStorage"))
        case "508" => Option((508, "LoopDetected"))
        case "509" => Option((509, "BandwidthLimitExceeded"))
        case "510" => Option((510, "NotExtended"))
        case "511" => Option((511, "NetworkAuthenticationRequired"))
        case "598" => Option((598, "NetworkReadTimeout"))
        case "599" => Option((599, "NetworkConnectTimeout"))
        case _ => None
      }
    }
  }

  object ServerTermInterp extends FunctionK[ServerTerm, Target] {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }
    def apply[T](term: ServerTerm[T]): Target[T] = term match {
      case ExtractOperations(paths) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"extractOperations(${paths})")
          routes <- paths.traverse { case (pathStr, path) =>
            for {
              _ <- Target.log.info("AkkaHttpServerGenerator", "server", "extractOperations")(s"(${pathStr}, ${path})")
              operationMap <- Target.fromOption(Option(path.getOperationMap), "No operations defined")
            } yield {
              operationMap.asScala.toList.map { case (httpMethod, operation) =>
                ServerRoute(pathStr, httpMethod, operation)
              }
            }
          }
        } yield routes.flatten

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
          className = pkg.map(_ ++ opPkg).getOrElse(opPkg)
        } yield className

      case BuildTracingFields(operation, resourceName, tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"buildTracingFields(${operation}, ${resourceName}, ${tracing})")
          res <- if (tracing) {
            for {
              operationId <- Target.fromOption(Option(operation.getOperationId()).map(splitOperationParts).map(_._2), "Missing operationId")
              label <- Target.fromOption(
                ScalaTracingLabel(operation)
                  .map(Lit.String(_))
                  .orElse(resourceName.lastOption.map(clientName => Lit.String(s"${clientName}:${operationId}"))), "Missing client name")
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
              valueType <- Option(resp.getSchema).traverse { prop =>
                for {
                  meta <- SwaggerUtil.propMeta(prop)
                  resolved <- SwaggerUtil.ResolvedType.resolve(meta, protocolElems)
                  SwaggerUtil.Resolved(baseType, _, baseDefaultValue) = resolved
                } yield baseType
              }
              responseTerm = Term.Name(s"${operationId}Response${statusCodeName.value}")
              responseName = Type.Name(s"${operationId}Response${statusCodeName.value}")
            } yield {
              valueType.fold[(Defn, Defn, Case)](
                ( q"case object $responseTerm                      extends $responseSuperType($statusCode)",
                  q"def $statusCodeName: $responseSuperType = $responseTerm",
                  p"case r: $responseTerm.type => scala.concurrent.Future.successful(Marshalling.Opaque { () => HttpResponse(r.statusCode) } :: Nil)"
                )
              ) { valueType =>
                ( q"case class  $responseName(value: $valueType) extends $responseSuperType($statusCode)",
                  q"def $statusCodeName(value: $valueType): $responseSuperType = $responseTerm(value)",
                  p"case r@$responseTerm(value) => Marshal(value).to[ResponseEntity].map { entity => Marshalling.Opaque { () => HttpResponse(r.statusCode, entity=entity) } :: Nil }"
                )
              }
            })
          }).sequence

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
          _ <- parameters.traverse(parameter => Target.log.debug("AkkaHttpServerGenerator", "server", "generateRoute", "parameter")(s"${parameter}"))

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
          _ <- routes.traverse(route => Target.log.debug("AkkaHttpServerGenerator", "server", "combineRouteTerms")(route.toString))
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

    def pathStrToAkka(basePath: Option[String], path: String, pathArgs: List[ScalaParameter]): Target[Term] = {

      def addTrailingSlashMatcher(trailingSlash: Boolean, term: Term.Apply): Term =
        if (trailingSlash) q"${term.copy(fun=Term.Name("pathPrefix"))} & pathEndOrSingleSlash"
        else term

      (basePath.getOrElse("") + path).stripPrefix("/") match {
        case "" => Target.pure(q"pathEndOrSingleSlash")
        case path =>
          for {
            pathDirective <- SwaggerUtil.paths.generateUrlAkkaPathExtractors(path, pathArgs)
          } yield pathDirective
      }
    }

    def directivesFromParams(
        required: Term => Type => Target[Term.Apply],
        multi: Term => Type => Target[Term.Apply],
        multiOpt: Term => Type => Target[Term.Apply],
        optional: Term => Type => Target[Term.Apply]
    )(params: List[ScalaParameter]): Target[Option[Term]] = {
      for {
        directives <- params.traverse { case ScalaParameter(_, param, _, argName, argType) =>
          param match {
            case param"$_: Option[Iterable[$tpe]]" => multiOpt(Lit.String(argName.value))(tpe)
            case param"$_: Option[Iterable[$tpe]] = $_" => multiOpt(Lit.String(argName.value))(tpe)
            case param"$_: Option[$tpe]" => optional(Lit.String(argName.value))(tpe)
            case param"$_: Option[$tpe] = $_" => optional(Lit.String(argName.value))(tpe)
            case param"$_: Iterable[$tpe]" => multi(Lit.String(argName.value))(tpe)
            case param"$_: Iterable[$tpe] = $_" => multi(Lit.String(argName.value))(tpe)
            case _ => required(Lit.String(argName.value))(argType)
          }
        }
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
