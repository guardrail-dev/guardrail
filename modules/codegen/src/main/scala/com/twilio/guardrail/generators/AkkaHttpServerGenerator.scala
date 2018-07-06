package com.twilio.guardrail
package generators

import _root_.io.swagger.models.HttpMethod
import cats.arrow.FunctionK
import cats.data.{ NonEmptyList, OptionT }
import cats.instances.all._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.SwaggerUtil
import com.twilio.guardrail.extract.{ ScalaPackage, ScalaTracingLabel, ServerRawResponse }
import com.twilio.guardrail.protocol.terms.server._

import scala.collection.JavaConverters._
import scala.meta._

object AkkaHttpServerGenerator {
  implicit class ExtendedUnzip[T1, T2, T3, T4, T5, T6, T7](xs: NonEmptyList[(T1, T2, T3, T4, T5, T6, T7)]) {
    def unzip7: (List[T1], List[T2], List[T3], List[T4], List[T5], List[T6], List[T7]) =
      xs.foldLeft(
        (List.empty[T1], List.empty[T2], List.empty[T3], List.empty[T4], List.empty[T5], List.empty[T6], List.empty[T7])
      ) {
        case ((v1a, v2a, v3a, v4a, v5a, v6a, v7a), (v1, v2, v3, v4, v5, v6, v7)) =>
          (v1a :+ v1, v2a :+ v2, v3a :+ v3, v4a :+ v4, v5a :+ v5, v6a :+ v6, v7a :+ v7)
      }
  }

  object HttpHelper {
    def apply(code: String): Option[(Int, String)] =
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
        case _     => None
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
          routes <- paths.traverse {
            case (pathStr, path) =>
              for {
                _            <- Target.log.info("AkkaHttpServerGenerator", "server", "extractOperations")(s"(${pathStr}, ${path})")
                operationMap <- Target.fromOption(Option(path.getOperationMap), "No operations defined")
              } yield {
                operationMap.asScala.toList.map {
                  case (httpMethod, operation) =>
                    ServerRoute(pathStr, httpMethod, operation)
                }
              }
          }
        } yield routes.flatten

      case GetClassName(operation) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getClassName(${operation})")

          pkg = ScalaPackage(operation)
            .map(_.split('.').toVector)
            .orElse({
              Option(operation.getTags).map { tags =>
                println(s"Warning: Using `tags` to define package membership is deprecated in favor of the `x-scala-package` vendor extension")
                tags.asScala
              }
            })
            .map(_.toList)
          opPkg = Option(operation.getOperationId())
            .map(splitOperationParts)
            .fold(List.empty[String])(_._1)
          className = pkg.map(_ ++ opPkg).getOrElse(opPkg)
        } yield className

      case BuildTracingFields(operation, resourceName, tracing) =>
        Target.getGeneratorSettings.flatMap { implicit gs =>
          for {
            _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"buildTracingFields(${operation}, ${resourceName}, ${tracing})")
            res <- if (tracing) {
              for {
                operationId <- Target.fromOption(Option(operation.getOperationId())
                                                   .map(splitOperationParts)
                                                   .map(_._2),
                                                 "Missing operationId")
                label <- Target.fromOption(
                  ScalaTracingLabel(operation)
                    .map(Lit.String(_))
                    .orElse(resourceName.lastOption.map(clientName => Lit.String(s"${clientName}:${operationId}"))),
                  "Missing client name"
                )
              } yield Some((ScalaParameter.fromParam(param"traceBuilder: TraceBuilder"), q"""trace(${label})"""))
            } else Target.pure(None)
          } yield res
        }

      case GenerateResponseDefinitions(operation, protocolElems) =>
        for {
          operationId <- Target.fromOption(Option(operation.getOperationId())
                                             .map(splitOperationParts)
                                             .map(_._2),
                                           "Missing operationId")
          responses <- Target
            .fromOption(Option(operation.getResponses).map(_.asScala), s"No responses defined for ${operationId}")
          responseSuperType = Type.Name(s"${operationId}Response")
          responseSuperTerm = Term.Name(s"${operationId}Response")

          instances <- responses
            .foldLeft[List[Target[(Defn, Defn, Case)]]](List.empty)({
              case (acc, (key, resp)) =>
                acc :+ (for {
                  httpCode <- Target.fromOption(HttpHelper(key), s"Unknown HTTP type: ${key}")
                  (code, friendlyName) = httpCode
                  statusCodeName       = Term.Name(friendlyName)
                  statusCode           = q"StatusCodes.${statusCodeName}"
                  valueType <- Option(resp.getSchema).traverse { prop =>
                    for {
                      meta <- SwaggerUtil.propMeta(prop)
                      resolved <- SwaggerUtil.ResolvedType
                        .resolve(meta, protocolElems)
                      SwaggerUtil
                        .Resolved(baseType, _, baseDefaultValue) = resolved
                    } yield baseType
                  }
                  responseTerm = Term.Name(s"${operationId}Response${statusCodeName.value}")
                  responseName = Type.Name(s"${operationId}Response${statusCodeName.value}")
                } yield {
                  valueType.fold[(Defn, Defn, Case)](
                    (q"case object $responseTerm                      extends $responseSuperType($statusCode)",
                     q"def $statusCodeName: $responseSuperType = $responseTerm",
                     p"case r: $responseTerm.type => scala.concurrent.Future.successful(Marshalling.Opaque { () => HttpResponse(r.statusCode) } :: Nil)")
                  ) { valueType =>
                    (q"case class  $responseName(value: $valueType) extends $responseSuperType($statusCode)",
                     q"def $statusCodeName(value: $valueType): $responseSuperType = $responseTerm(value)",
                     p"case r@$responseTerm(value) => Marshal(value).to[ResponseEntity].map { entity => Marshalling.Opaque { () => HttpResponse(r.statusCode, entity=entity) } :: Nil }")
                  }
                })
            })
            .sequence

          (terms, aliases, marshallers) = instances.unzip3

          convenienceConstructors = aliases.flatMap({
            case q"def $name(value: $tpe): $_ = $_" => tpe.map { (_, name) }
            case _                                  => None
          })

          implicitHelpers = convenienceConstructors
            .groupBy(_._1)
            .flatMap({
              case (tpe, (_, name) :: Nil) =>
                Some(q"implicit def ${Term
                  .Name(s"${name.value}Ev")}(value: ${tpe}): ${responseSuperType} = ${name}(value)")
              case _ => None
            })
            .toList

          companion = q"""
            object ${responseSuperTerm} {
              implicit val ${Pat
            .Var(Term.Name(s"${operationId}TRM"))}: ToResponseMarshaller[${responseSuperType}] = Marshaller { implicit ec => resp => ${Term
            .Name(s"${operationId}TR")}(resp) }
              implicit def ${Term
            .Name(s"${operationId}TR")}(value: ${responseSuperType})(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] =
                ${Term.Match(Term.Name("value"), marshallers)}

              def apply[T](value: T)(implicit ev: T => ${responseSuperType}): ${responseSuperType} = ev(value)

              ..${implicitHelpers}

              ..${aliases}
            }
          """

        } yield
          List[Defn](
            q"sealed abstract class ${responseSuperType}(val statusCode: StatusCode)"
          ) ++ terms ++ List[Defn](
            companion
          )

      case GenerateRoute(resourceName, basePath, route @ ServerRoute(path, method, operation), tracingFields, responseDefinitions, protocolElems) =>
        // Generate the pair of the Handler method and the actual call to `complete(...)`
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"generateRoute(${resourceName}, ${basePath}, ${route}, ${tracingFields})")
          operationId <- Target.fromOption(Option(operation.getOperationId())
                                             .map(splitOperationParts)
                                             .map(_._2),
                                           "Missing operationId")
          parameters <- Option(operation.getParameters)
            .map(_.asScala.toList)
            .map(ScalaParameter.fromParameters(protocolElems))
            .getOrElse(Target.pure(List.empty[ScalaParameter]))
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server", "generateRoute")("Parameters:")
          _ <- parameters.traverse(parameter => Target.log.debug("AkkaHttpServerGenerator", "server", "generateRoute", "parameter")(s"${parameter}"))

          filterParamBy = ScalaParameter.filterParams(parameters)
          bodyArgs      = filterParamBy("body").headOption
          formArgs = filterParamBy("formData").toList.map({ x =>
            x.withType(
              if (x.isFile) {
                val fileParams = List(t"File", t"Option[String]", t"ContentType") ++ x.hashAlgorithm.map(Function.const(t"String"))
                if (x.required) {
                  t"(..${fileParams})"
                } else {
                  t"Option[(..${fileParams})]"
                }
              } else {
                x.argType
              }
            )
          })
          headerArgs = filterParamBy("header").toList
          pathArgs   = filterParamBy("path").toList
          qsArgs     = filterParamBy("query").toList

          akkaMethod <- httpMethodToAkka(method)
          akkaPath   <- pathStrToAkka(basePath, path, pathArgs)
          akkaQs     <- qsToAkka(qsArgs)
          akkaBody   <- bodyToAkka(bodyArgs)
          asyncFormProcessing = formArgs.exists(_.isFile)
          akkaForm_ <- if (asyncFormProcessing) { asyncFormToAkka(operationId)(formArgs) } else { formToAkka(formArgs).map((_, List.empty[Defn])) }
          (akkaForm, handlerDefinitions) = akkaForm_
          akkaHeaders <- headersToAkka(headerArgs)
        } yield {
          val (responseCompanionTerm, responseCompanionType) =
            (Term.Name(s"${operationId}Response"), Type.Name(s"${operationId}Response"))
          val responseType = ServerRawResponse(operation)
            .filter(_ == true)
            .fold[Type](t"${Term.Name(resourceName)}.${responseCompanionType}")(Function.const(t"HttpResponse"))
          val orderedParameters: List[List[ScalaParameter]] = List((pathArgs ++ qsArgs ++ bodyArgs ++ formArgs ++ headerArgs).toList) ++ tracingFields
            .map(_._1)
            .map(List(_))

          val entityProcessor = akkaBody.orElse(akkaForm).getOrElse(q"discardEntity")
          val fullRouteMatcher =
            List[Option[Term]](Some(akkaMethod), Some(akkaPath), akkaQs, Some(entityProcessor), akkaHeaders, tracingFields.map(_._2)).flatten.reduceLeft {
              (a, n) =>
                q"${a} & ${n}"
            }
          val handlerCallArgs: List[List[Term.Name]] = List(List(responseCompanionTerm)) ++ orderedParameters.map(_.map(_.paramName))
          val fullRoute: Term.Apply = orderedParameters match {
            case List(List()) =>
              q"""
              ${fullRouteMatcher} {
                complete(handler.${Term
                .Name(operationId)}(...${handlerCallArgs}))
              }
              """
            case params =>
              q"""
              ${fullRouteMatcher} { (..${params.flatten.map(p => param"${p.paramName}")}) =>
                complete(handler.${Term
                .Name(operationId)}(...${handlerCallArgs}))
              }
              """
          }

          val respond: List[List[Term.Param]] = List(List(param"respond: ${Term.Name(resourceName)}.${responseCompanionTerm}.type"))

          val params: List[List[Term.Param]] = respond ++ orderedParameters.map(
            _.map(
              scalaParam =>
                scalaParam.param.copy(
                  decltpe =
                    (
                      if (scalaParam.isFile) {
                        val fileParams = List(t"File", t"Option[String]", t"ContentType") ++ scalaParam.hashAlgorithm.map(Function.const(t"String"))
                        if (scalaParam.required) {
                          Some(t"(..${fileParams})")
                        } else {
                          Some(t"Option[(..${fileParams})]")
                        }
                      } else {
                        scalaParam.param.decltpe
                      }
                    )
              )
            )
          )
          RenderedRoute(
            fullRoute,
            q"""
              def ${Term
              .Name(operationId)}(...${params}): scala.concurrent.Future[${responseType}]
            """,
            responseDefinitions,
            List.empty,
            handlerDefinitions
          )
        }

      case CombineRouteTerms(terms) =>
        for {
          _      <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"combineRouteTerms(<${terms.length} routes>)")
          routes <- Target.fromOption(NonEmptyList.fromList(terms), "Generated no routes, no source to generate")
          _      <- routes.traverse(route => Target.log.debug("AkkaHttpServerGenerator", "server", "combineRouteTerms")(route.toString))
        } yield routes.tail.foldLeft(routes.head) { case (a, n) => q"${a} ~ ${n}" }

      case RenderHandler(handlerName, methodSigs, handlerDefinitions) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"renderHandler(${handlerName}, ${methodSigs}")
        } yield q"""
          trait ${Type.Name(handlerName)} {
            ..${methodSigs ++ handlerDefinitions}
          }
        """

      case GetExtraRouteParams(tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getExtraRouteParams(${tracing})")
          res <- if (tracing) {
            Target.pure(List(param"""trace: String => Directive1[TraceBuilder]"""))
          } else Target.pure(List.empty)
        } yield res

      case RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"renderClass(${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams})")
          routesParams = List(param"handler: ${Type.Name(handlerName)}") ++ extraRouteParams
        } yield q"""
          object ${Term.Name(resourceName)} {
            def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap({ req => req.discardEntityBytes().future; Directive.Empty })
            implicit def jsonFSU[T: io.circe.Decoder]: Unmarshaller[String, T] = Unmarshaller[String, T]({
              implicit ev => string =>
               io.circe.Json.fromString(string).as[T]
                .left.flatMap(err => io.circe.jawn.parse(string).flatMap(_.as[T]))
                .fold(scala.concurrent.Future.failed _, scala.concurrent.Future.successful _)
            })

            ..${supportDefinitions};
            def routes(..${routesParams})(implicit mat: akka.stream.Materializer): Route = {
              ${combinedRouteTerms}
            }

            ..${responseDefinitions}
          }
        """

      case GetExtraImports(tracing) =>
        for {
          _ <- Target.log.debug("AkkaHttpServerGenerator", "server")(s"getExtraImports(${tracing})")
        } yield
          if (tracing) List(q"import akka.http.scaladsl.server.Directive1")
          else List.empty
    }

    def httpMethodToAkka(method: HttpMethod): Target[Term] = method match {
      case HttpMethod.DELETE => Target.pure(q"delete")
      case HttpMethod.GET    => Target.pure(q"get")
      case HttpMethod.PATCH  => Target.pure(q"patch")
      case HttpMethod.POST   => Target.pure(q"post")
      case HttpMethod.PUT    => Target.pure(q"put")
      case other             => Target.error(s"Unknown method: ${other}")
    }

    def pathStrToAkka(basePath: Option[String], path: String, pathArgs: List[ScalaParameter]): Target[Term] = {

      def addTrailingSlashMatcher(trailingSlash: Boolean, term: Term.Apply): Term =
        if (trailingSlash)
          q"${term.copy(fun = Term.Name("pathPrefix"))} & pathEndOrSingleSlash"
        else term

      (basePath.getOrElse("") + path).stripPrefix("/") match {
        case "" => Target.pure(q"pathEndOrSingleSlash")
        case path =>
          for {
            pathDirective <- SwaggerUtil.paths
              .generateUrlAkkaPathExtractors(path, pathArgs)
          } yield pathDirective
      }
    }

    def directivesFromParams(
        required: Term => Type => Target[Term.Apply],
        multi: Term => Type => Target[Term.Apply],
        multiOpt: Term => Type => Target[Term.Apply],
        optional: Term => Type => Target[Term.Apply]
    )(params: List[ScalaParameter]): Target[Option[Term]] =
      for {
        directives <- params.traverse {
          case ScalaParameter(_, param, _, argName, argType) =>
            param match {
              case param"$_: Option[Iterable[$tpe]]" =>
                multiOpt(argName.toLit)(tpe)
              case param"$_: Option[Iterable[$tpe]] = $_" =>
                multiOpt(argName.toLit)(tpe)
              case param"$_: Option[$tpe]" =>
                optional(argName.toLit)(tpe)
              case param"$_: Option[$tpe] = $_" =>
                optional(argName.toLit)(tpe)
              case param"$_: Iterable[$tpe]" =>
                multi(argName.toLit)(tpe)
              case param"$_: Iterable[$tpe] = $_" =>
                multi(argName.toLit)(tpe)
              case _ => required(argName.toLit)(argType)
            }
        }
      } yield
        directives match {
          case Nil => Option.empty
          case x :: xs =>
            Some(xs.foldLeft[Term](x) { case (a, n) => q"${a} & ${n}" })
        }

    def bodyToAkka(body: Option[ScalaParameter]): Target[Option[Term]] =
      Target.pure(
        body.map {
          case ScalaParameter(_, _, _, _, argType) =>
            q"entity(as[${argType}])"
        }
      )

    def headersToAkka: List[ScalaParameter] => Target[Option[Term]] =
      directivesFromParams(
        arg => tpe => Target.pure(q"headerValueByName(${arg})"),
        arg => tpe => Target.error(s"Unsupported Iterable[${arg}]"),
        arg => tpe => Target.error(s"Unsupported Option[Iterable[${arg}]]"),
        arg => tpe => Target.pure(q"optionalHeaderValueByName(${arg})")
      ) _

    def qsToAkka: List[ScalaParameter] => Target[Option[Term]] =
      directivesFromParams(
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}])"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].*)"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].*).map(Option.apply _)"),
        arg => tpe => Target.pure(q"parameter(Symbol(${arg}).as[${tpe}].?)")
      ) _

    def formToAkka: List[ScalaParameter] => Target[Option[Term]] =
      directivesFromParams(
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}])"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].*)"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].*).map(Option.apply _)"),
        arg => tpe => Target.pure(q"formField(Symbol(${arg}).as[${tpe}].?)")
      ) _

    def asyncFormToAkka(operationId: String): List[ScalaParameter] => Target[(Option[Term], List[Stat])] = { params =>
      class Binding(value: String) {
        def toPat: Pat        = if (value.nonEmpty && ('A'.to('Z').contains(value(0)))) toTerm else toVar
        def toVar: Pat.Var    = Pat.Var(toTerm)
        def toTerm: Term.Name = Term.Name(value)
        def toType: Type.Name = Type.Name(value)
      }
      (for {
        params <- NonEmptyList.fromList(params)
      } yield {
        val partsTerm = Term.Name(s"${operationId}Parts")
        val value = params
          .map({
            case rawParameter @ ScalaParameter(a, param, paramName, argName, argType) =>
              val containerName    = new Binding(paramName.value)
              val unmarshallerName = new Binding(s"Unmarshal${paramName.value}Part")
              val binding          = new Binding(paramName.value)
              val collected        = new Binding(s"${paramName.value}O")

              val isFile: Boolean = rawParameter.isFile
              val (isOptional, realType): (Boolean, Type) = argType match {
                case t"Option[$x]" => (true, x)
                case x             => (false, x)
              }
              val (unmarshaller, caseMatch, grabHead) = isFile match {
                case true =>
                  (
                    q"""
                        val ${unmarshallerName.toVar}: Unmarshaller[Multipart.FormData.BodyPart, ${Type
                      .Select(partsTerm, containerName.toType)}] = (
                            handler.${Term.Name(s"${operationId}UnmarshalToFile")}[${rawParameter.hashAlgorithm
                      .fold(t"Option")(Function.const(t"Id"))}](${rawParameter.hashAlgorithm.fold[Term](q"None")(x => Lit.String(x))}, handler.${Term
                      .Name(s"${operationId}MapFileField")}(_, _, _))
                              .map({ case (v1, v2, v3, v4) =>
                                ${Term.Select(partsTerm, containerName.toTerm)}((..${List(q"v1", q"v2", q"v3") ++ rawParameter.hashAlgorithm
                      .map(Function.const(q"v4"))}))
                              })
                            )
                      """,
                    Case(
                      argName.toLit,
                      None,
                      q"""
                          SafeUnmarshaller(AccumulatingUnmarshaller(fileReferences, ${unmarshallerName.toTerm})(_.value._1)).apply(part)
                      """
                    ),
                    q"""
                        val ${collected.toVar} = successes.collectFirst(${Term.PartialFunction(
                      List(
                        Case(
                          Pat.Extract(Term.Select(partsTerm, containerName.toTerm),
                                      List(p"((..${List(p"v1", p"v2", p"v3") ++ rawParameter.hashAlgorithm.map(Function.const(p"v4"))}))")),
                          None,
                          q"(..${List(q"v1", q"v2", q"v3") ++ rawParameter.hashAlgorithm.map(Function.const(q"v4"))})"
                        )
                      )
                    )})
                      """
                  )
                case false =>
                  (
                    q"""
                        val ${unmarshallerName.toVar}: Unmarshaller[Multipart.FormData.BodyPart, ${Type
                      .Select(partsTerm, containerName.toType)}] = Unmarshaller { implicit executionContext => part =>
                          Unmarshaller.firstOf(
                            implicitly[Unmarshaller[Multipart.FormData.BodyPart, ${realType}]],
                            MFDBPviaFSU(Unmarshaller.stringUnmarshaller)
                          ).apply(part).map(${Term
                      .Select(partsTerm, containerName.toTerm)}.apply)
                        }
                      """,
                    Case(argName.toLit, None, q"""
                          SafeUnmarshaller(${unmarshallerName.toTerm}).apply(part)
                      """),
                    q"""
                        val ${collected.toVar} = successes.collectFirst(${Term.PartialFunction(
                      List(Case(Pat.Extract(Term.Select(partsTerm, containerName.toTerm), List(Pat.Var(Term.Name("v1")))), None, Term.Name("v1")))
                    )})
                      """
                  )
              }

              val partContainer = q"case class ${containerName.toType}(..${List(param"value: ${realType}")}) extends Part"

              val unpacker = if (isOptional) {
                enumerator""" ${binding.toVar} <- Either.right[MissingFormFieldRejection, ${argType}](${collected.toTerm}) """
              } else {
                enumerator""" ${binding.toVar} <- ${collected.toTerm}.toRight(MissingFormFieldRejection(${argName.toLit})) """
              }

              (partContainer, unmarshaller, caseMatch, (binding, collected), unpacker, argType, grabHead)
          })
          .unzip7

        val (partContainers, unmarshallers, matchers, _terms, unpacks, termTypes, grabHeads) = value
        val (termPatterns, optionalTermPatterns)                                             = _terms.unzip
        val optionalTuple                                                                    = p"(..${optionalTermPatterns.map(_.toPat)})"

        val _trait              = q"sealed trait Part"
        val ignoredPart         = q"case class IgnoredPart(unit: Unit) extends Part"
        val ignoredUnmarshaller = p"""
          case _ =>
            SafeUnmarshaller(
            implicitly[Unmarshaller[Multipart.FormData.BodyPart, Unit]]
                                                        .map(${partsTerm}.IgnoredPart.apply(_))
          ).apply(part)
        """

        val fieldNames = q"""Set[String](..${params.toList.map(_.argName.toLit)})"""
        val optionalTypes = t"(..${termTypes.map({
          case x @ t"Option[$_]" => x
          case x                 => t"Option[$x]"
        })})"
        val unpackedTypes = t"(..${termTypes})"

        val allCases: List[Case] = matchers ++ List(ignoredUnmarshaller)

        val handlerDefinitions = List(
          q"""
          def ${Term.Name(s"${operationId}MapFileField")}(fieldName: String, fileName: Option[String], contentType: ContentType): File
          """,
          q"""
            def ${Term
            .Name(s"${operationId}UnmarshalToFile")}[F[_]: Functor](hashType: F[String], destFn: (String, Option[String], ContentType) => File)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, (File, Option[String], ContentType, F[String])] = Unmarshaller { implicit executionContext => part =>
              val dest = destFn(part.name, part.filename, part.entity.contentType)
              val messageDigest = hashType.map(MessageDigest.getInstance(_))
              val fileSink: Sink[ByteString,Future[IOResult]] = FileIO.toPath(dest.toPath).contramap[ByteString] { chunk => val _ = messageDigest.map(_.update(chunk.toArray[Byte])); chunk }

              part.entity.dataBytes.toMat(fileSink)(Keep.right).run()
                .transform({
                  case IOResult(_, Success(_)) =>
                    val hash = messageDigest.map(md => javax.xml.bind.DatatypeConverter.printHexBinary(md.digest()).toLowerCase(java.util.Locale.US))
                    (dest, part.filename, part.entity.contentType, hash)
                  case IOResult(_, Failure(t)) =>
                    throw t
                }, { case t =>
                  dest.delete()
                  t
                })
            }
          """
        )

        val directive: Term = (
          q"""
          ({
            object ${partsTerm} {
              ..${List(
            _trait,
            ignoredPart
          ) ++ partContainers}
            }

            ..${unmarshallers};

            val fileReferences = new AtomicReference(List.empty[File])
            (extractExecutionContext.flatMap { implicit executionContext =>
              extractMaterializer.flatMap { implicit mat =>
                entity(as[Multipart.FormData]).flatMap { formData =>
                  val collectedPartsF: Future[Either[Throwable, ${optionalTypes}]] = for {
                    results <- formData.parts
                      .mapConcat({ part =>
                        if (${fieldNames}.contains(part.name)) part :: Nil
                        else {
                          part.entity.discardBytes()
                          Nil
                        }
                      }).mapAsync(1)({ part =>
                        ${Term.Match(q"part.name", allCases)}
                      }).toMat(Sink.seq[Either[Throwable, ${Type.Select(partsTerm, Type.Name("Part"))}]])(Keep.right).run()
                    } yield {
                      results.toList.sequence.map({ successes =>
                        ..${grabHeads}

                        (..${optionalTermPatterns.map(_.toTerm)})
                      })
                    }

                    (
                      handleExceptions(ExceptionHandler {
                        case e: Throwable =>
                          fileReferences.get().foreach(_.delete())
                          throw e
                      }) & handleRejections({ rejections: scala.collection.immutable.Seq[Rejection] =>
                        fileReferences.get().foreach(_.delete())
                        None
                      }) & mapResponse({ resp =>
                        fileReferences.get().foreach(_.delete())
                        resp
                      })
                  ) & onSuccess(collectedPartsF)

                }
              }
            }).flatMap(_.fold(t => throw t, ${Term.PartialFunction(
            List(
              Case(
                optionalTuple,
                None,
                q"""
              val maybe: Either[Rejection, ${unpackedTypes}] = for {
                ..${unpacks}
              } yield {
                (..${termPatterns.map(_.toTerm)})
              }
              maybe.fold(reject(_), tprovide(_))
            """
              )
            )
          )}))
          }: Directive[${unpackedTypes}])
        """
        )

        (directive, handlerDefinitions)
      }).fold(Target.pure((Option.empty[Term], List.empty[Stat])))({ case (v1, v2) => Target.pure((Option(v1), v2)) })
    }
  }
}
