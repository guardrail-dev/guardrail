package dev.guardrail.generators.Scala

import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import cats.Traverse
import dev.guardrail.{ CustomExtractionField, RenderedRoutes, StrictProtocolElems, SwaggerUtil, Target, TracingField, UserError }
import dev.guardrail.core.Tracker
import dev.guardrail.extract.{ ServerRawResponse, TracingLabel }
import dev.guardrail.generators.{ LanguageParameter, LanguageParameters }
import dev.guardrail.generators.syntax._
import dev.guardrail.generators.operations.TracingLabelFormatter
import dev.guardrail.generators.syntax.Scala._
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.{ ContentType, Header, Response, Responses }
import dev.guardrail.protocol.terms.server._
import dev.guardrail.shims._
import dev.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }

import scala.meta._
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import _root_.io.swagger.v3.oas.models.Operation

object Http4sServerGenerator {
  def ServerTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ServerTerms[ScalaLanguage, Target] =
    new ServerTermInterp
  class ServerTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ServerTerms[ScalaLanguage, Target] {
    val customExtractionTypeName: Type.Name = Type.Name("E")

    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[ScalaLanguage],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]]
    ) =
      Target.pure(Http4sHelper.generateResponseDefinitions(responseClsName, responses, protocolElems))

    def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean) =
      for {
        _ <- Target.log.debug(s"buildCustomExtractionFields(${operation.unwrapTracker.showNotNull}, ${resourceName}, ${customExtraction})")
        res <- if (customExtraction) {
          for {
            operationId <- operation
              .downField("operationId", _.getOperationId())
              .raiseErrorIfEmpty("Missing operationId")
            operationId_ = Lit.String(splitOperationParts(operationId.unwrapTracker)._2)
          } yield Some(
            CustomExtractionField[ScalaLanguage](
              LanguageParameter.fromParam(param"extracted: $customExtractionTypeName"),
              q"""customExtract(${operationId_})"""
            )
          )
        } else Target.pure(None)
      } yield res

    def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean) =
      Target.log.function("buildTracingFields")(for {
        _ <- Target.log.debug(s"Args: ${operation}, ${resourceName}, ${tracing}")
        res <- if (tracing) {
          for {
            operationId <- operation
              .downField("operationId", _.getOperationId())
              .map(_.map(splitOperationParts(_)._2))
              .raiseErrorIfEmpty("Missing operationId")
            label <- Target.fromOption[Lit.String](
              TracingLabel(operation)
                .map(Lit.String(_))
                .orElse(resourceName.lastOption.map(clientName => TracingLabelFormatter(clientName, operationId.unwrapTracker).toLit)),
              UserError(s"Missing client name (${operation.showHistory})")
            )
          } yield Some(TracingField[ScalaLanguage](LanguageParameter.fromParam(param"traceBuilder: TraceBuilder[F]"), q"""trace(${label})"""))
        } else Target.pure(None)
      } yield res)

    def generateRoutes(
        tracing: Boolean,
        resourceName: String,
        handlerName: String,
        basePath: Option[String],
        routes: List[GenerateRouteMeta[ScalaLanguage]],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]],
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ) =
      for {
        renderedRoutes <- routes
          .traverse {
            case GenerateRouteMeta(
                operationId,
                methodName,
                responseClsName,
                customExtractionFields,
                tracingFields,
                sr @ RouteMeta(path, method, operation, securityRequirements),
                parameters,
                responses
                ) =>
              generateRoute(resourceName, basePath, methodName, responseClsName, sr, customExtractionFields, tracingFields, parameters, responses)
          }
          .map(_.flatten.sortBy(_.methodName))
        routeTerms = renderedRoutes.map(_.route)
        combinedRouteTerms <- combineRouteTerms(routeTerms)
        methodSigs = renderedRoutes.map(_.methodSig)
      } yield {
        RenderedRoutes[ScalaLanguage](
          List(combinedRouteTerms),
          List.empty,
          methodSigs,
          renderedRoutes
            .flatMap(_.supportDefinitions)
            .groupBy(_.structure)
            .flatMap(_._2.headOption)
            .toList
            .sortBy(_.toString()), // Only unique supportDefinitions by structure
          renderedRoutes.flatMap(_.handlerDefinitions)
        )
      }

    def renderHandler(
        handlerName: String,
        methodSigs: List[scala.meta.Decl.Def],
        handlerDefinitions: List[scala.meta.Stat],
        responseDefinitions: List[scala.meta.Defn],
        customExtraction: Boolean
    ) =
      Target.log.function("renderHandler")(for {
        _ <- Target.log.debug(s"Args: ${handlerName}, ${methodSigs}")
        extractType = List(tparam"-$customExtractionTypeName").filter(_ => customExtraction)
        tParams     = List(tparam"F[_]") ++ extractType
      } yield q"""
      trait ${Type.Name(handlerName)}[..$tParams] {
        ..${methodSigs ++ handlerDefinitions}
      }
    """)

    def getExtraRouteParams(customExtraction: Boolean, tracing: Boolean) =
      Target.log.function("getExtraRouteParams")(for {
        _ <- Target.log.debug(s"getExtraRouteParams(${tracing})")
        mapRoute = param"""mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r"""
        customExtraction_ = if (customExtraction) {
          Option(param"""customExtract: String => Request[F] => $customExtractionTypeName""")
        } else Option.empty
        tracing_ = if (tracing) {
          Option(param"""trace: String => Request[F] => TraceBuilder[F]""")
        } else Option.empty
      } yield customExtraction_.toList ::: tracing_.toList ::: List(mapRoute))

    def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ) =
      Target.pure(List.empty)

    def renderClass(
        resourceName: String,
        handlerName: String,
        annotations: List[scala.meta.Mod.Annot],
        combinedRouteTerms: List[scala.meta.Stat],
        extraRouteParams: List[scala.meta.Term.Param],
        responseDefinitions: List[scala.meta.Defn],
        supportDefinitions: List[scala.meta.Defn],
        customExtraction: Boolean
    ): Target[List[Defn]] =
      Target.log.function("renderClass")(for {
        _ <- Target.log.debug(s"Args: ${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams}")
        extractType     = List(customExtractionTypeName).map(x => tparam"$x").filter(_ => customExtraction)
        resourceTParams = List(tparam"F[_]") ++ extractType
        handlerTParams  = List(Type.Name("F")) ++ List(customExtractionTypeName).filter(_ => customExtraction)
        routesParams    = List(param"handler: ${Type.Name(handlerName)}[..$handlerTParams]")
      } yield q"""
        class ${Type.Name(resourceName)}[..$resourceTParams](..$extraRouteParams)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {

          ..${supportDefinitions};
          def routes(..${routesParams}): HttpRoutes[F] = HttpRoutes.of {
            ..${combinedRouteTerms}
          }
        }
      """ +: responseDefinitions)

    def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]) =
      Target.log.function("getExtraImports")(
        for {
          _ <- Target.log.debug(s"Args: ${tracing}")
        } yield List(
          q"import org.http4s.circe.CirceInstances",
          q"import org.http4s.dsl.Http4sDsl",
          q"import fs2.text._"
        )
      )

    def httpMethodToHttp4s(method: HttpMethod): Target[Term.Name] = method match {
      case HttpMethod.DELETE => Target.pure(Term.Name("DELETE"))
      case HttpMethod.GET    => Target.pure(Term.Name("GET"))
      case HttpMethod.PATCH  => Target.pure(Term.Name("PATCH"))
      case HttpMethod.POST   => Target.pure(Term.Name("POST"))
      case HttpMethod.PUT    => Target.pure(Term.Name("PUT"))
      case other             => Target.raiseUserError(s"Unknown method: ${other}")
    }

    def pathStrToHttp4s(basePath: Option[String], path: Tracker[String], pathArgs: List[LanguageParameter[ScalaLanguage]]): Target[(Pat, Option[Pat])] =
      (basePath.getOrElse("") + path.unwrapTracker).stripPrefix("/") match {
        case "" => Target.pure((p"${Term.Name("Root")}", None))
        case finalPath =>
          for {
            pathDirective <- SwaggerUtil.paths
              .generateUrlHttp4sPathExtractors(Tracker.cloneHistory(path, finalPath), pathArgs)
          } yield pathDirective
      }

    def directivesFromParams[T](
        required: LanguageParameter[ScalaLanguage] => Type => Target[T],
        multi: LanguageParameter[ScalaLanguage] => Type => (Term => Term) => Target[T],
        multiOpt: LanguageParameter[ScalaLanguage] => Type => (Term => Term) => Target[T],
        optional: LanguageParameter[ScalaLanguage] => Type => Target[T]
    )(params: List[LanguageParameter[ScalaLanguage]]): Target[List[T]] =
      for {
        directives <- params.traverse[Target, T] {
          case scalaParam @ LanguageParameter(_, param, _, _, argType) =>
            val containerTransformations = Map[String, Term => Term](
              "Iterable"   -> identity _,
              "List"       -> (term => q"$term.toList"),
              "Vector"     -> (term => q"$term.toVector"),
              "Seq"        -> (term => q"$term.toSeq"),
              "IndexedSeq" -> (term => q"$term.toIndexedSeq")
            )

            param match {
              case param"$_: Option[$container[$tpe]]" if containerTransformations.contains(container.syntax) =>
                multiOpt(scalaParam)(tpe)(containerTransformations(container.syntax))
              case param"$_: Option[$container[$tpe]] = $_" if containerTransformations.contains(container.syntax) =>
                multiOpt(scalaParam)(tpe)(containerTransformations(container.syntax))
              case param"$_: Option[$tpe]" =>
                optional(scalaParam)(tpe)
              case param"$_: Option[$tpe] = $_" =>
                optional(scalaParam)(tpe)
              case param"$_: $container[$tpe]" if containerTransformations.contains(container.syntax) =>
                multi(scalaParam)(tpe)(containerTransformations(container.syntax))
              case param"$_: $container[$tpe] = $_" if containerTransformations.contains(container.syntax) =>
                multi(scalaParam)(tpe)(containerTransformations(container.syntax))
              case _ => required(scalaParam)(argType)
            }
        }
      } yield directives

    def bodyToHttp4s(methodName: String, body: Option[LanguageParameter[ScalaLanguage]]): Target[Option[Term => Term]] =
      Target.pure(
        body.map {
          case LanguageParameter(_, _, paramName, _, _) =>
            content => q"req.decodeWith(${Term.Name(s"${methodName.uncapitalized}Decoder")}, strict = false) { ${param"$paramName"} => $content }"
        }
      )

    case class Param(generator: Option[Enumerator.Generator], matcher: Option[(Term, Pat)], handlerCallArg: Term)

    def headersToHttp4s: List[LanguageParameter[ScalaLanguage]] => Target[List[Param]] =
      directivesFromParams(
        arg => {
          case t"String" =>
            Target.pure(Param(None, Some((q"req.headers.get(${arg.argName.toLit}.ci).map(_.value)", p"Some(${Pat.Var(arg.paramName)})")), arg.paramName))
          case tpe =>
            Target.pure(
              Param(
                None,
                Some((q"req.headers.get(${arg.argName.toLit}.ci).map(_.value).map(Json.fromString(_).as[$tpe])", p"Some(Right(${Pat.Var(arg.paramName)}))")),
                arg.paramName
              )
            )
        },
        arg => _ => _ => Target.raiseUserError(s"Unsupported Iterable[${arg}"),
        arg => _ => _ => Target.raiseUserError(s"Unsupported Option[Iterable[${arg}]]"),
        arg => {
          case t"String" => Target.pure(Param(None, None, q"req.headers.get(${arg.argName.toLit}.ci).map(_.value)"))
          case tpe =>
            Target.pure(
              Param(
                None,
                Some((q"req.headers.get(${arg.argName.toLit}.ci).map(_.value).map(Json.fromString(_).as[$tpe]).sequence", p"Right(${Pat.Var(arg.paramName)})")),
                arg.paramName
              )
            )
        }
      )

    def qsToHttp4s(methodName: String): List[LanguageParameter[ScalaLanguage]] => Target[Option[Pat]] =
      params =>
        directivesFromParams(
          arg => _ => Target.pure(p"${Term.Name(s"${methodName.capitalize}${arg.argName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)})"),
          arg => _ => _ => Target.pure(p"${Term.Name(s"${methodName.capitalize}${arg.argName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)})"),
          arg => _ => _ => Target.pure(p"${Term.Name(s"${methodName.capitalize}${arg.argName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)})"),
          arg => _ => Target.pure(p"${Term.Name(s"${methodName.capitalize}${arg.argName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)})")
        )(params).map {
          case Nil => Option.empty
          case x :: xs =>
            Some(xs.foldLeft[Pat](x) { case (a, n) => p"${a} +& ${n}" })
        }

    def formToHttp4s: List[LanguageParameter[ScalaLanguage]] => Target[List[Param]] =
      directivesFromParams(
        arg => {
          case t"String" =>
            Target.pure(
              Param(None, Some((q"urlForm.values.get(${arg.argName.toLit}).flatMap(_.headOption)", p"Some(${Pat.Var(arg.paramName)})")), arg.paramName)
            )
          case tpe =>
            Target.pure(
              Param(
                None,
                Some(
                  (
                    q"urlForm.values.get(${arg.argName.toLit}).flatMap(_.headOption).map(Json.fromString(_).as[$tpe])",
                    p"Some(Right(${Pat.Var(arg.paramName)}))"
                  )
                ),
                arg.paramName
              )
            )
        },
        arg => {
          case t"String" =>
            _ => Target.pure(Param(None, Some((q"urlForm.values.get(${arg.argName.toLit})", p"Some(${Pat.Var(arg.paramName)})")), q"${arg.paramName}.toList"))
          case tpe =>
            _ =>
              Target.pure(
                Param(
                  None,
                  Some(
                    (
                      q"urlForm.values.get(${arg.argName.toLit}).flatMap(_.toList).traverse(Json.fromString(_).as[$tpe])",
                      p"Some(Right(${Pat.Var(arg.paramName)}))"
                    )
                  ),
                  arg.paramName
                )
              )
        },
        arg => {
          case t"String" => _ => Target.pure(Param(None, None, q"urlForm.values.get(${arg.argName.toLit}).map(_.toList)"))
          case tpe =>
            _ =>
              Target.pure(
                Param(
                  None,
                  Some(
                    (
                      q"urlForm.values.get(${arg.argName.toLit}).flatMap(_.toList).map(Json.fromString(_).as[$tpe]).sequence.sequence",
                      p"Right(${Pat.Var(arg.paramName)})"
                    )
                  ),
                  arg.paramName
                )
              )
        },
        arg => {
          case t"String" =>
            Target.pure(Param(None, Some((q"urlForm.values.get(${arg.argName.toLit}).traverse(_.toList)", p"List(${Pat.Var(arg.paramName)})")), arg.paramName))
          case tpe =>
            Target.pure(
              Param(
                None,
                Some(
                  (
                    q"urlForm.values.get(${arg.argName.toLit}).traverse(_.toList).map(_.traverse(Json.fromString(_).as[$tpe]))",
                    p"List(Right(${Pat.Var(arg.paramName)}))"
                  )
                ),
                arg.paramName
              )
            )
        }
      )

    def asyncFormToHttp4s(methodName: String): List[LanguageParameter[ScalaLanguage]] => Target[List[Param]] =
      directivesFromParams(
        arg =>
          elemType =>
            if (arg.isFile) {
              Target.pure(
                Param(
                  None,
                  Some((q"multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body)", p"Some(${Pat.Var(arg.paramName)})")),
                  arg.paramName
                )
              )
            } else
              elemType match {
                case t"String" =>
                  Target.pure(
                    Param(
                      Some(
                        enumerator"${Pat.Var(Term.Name(s"${arg.argName.value}Option"))} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid).sequence"
                      ),
                      Some(
                        (
                          Term.Name(s"${arg.argName.value}Option"),
                          p"Some(${Pat.Var(arg.paramName)})"
                        )
                      ),
                      arg.paramName
                    )
                  )
                case tpe =>
                  Target.pure(
                    Param(
                      Some(
                        enumerator"${Pat.Var(Term.Name(s"${arg.argName.value}Option"))} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence"
                      ),
                      Some(
                        (
                          Term.Name(s"${arg.argName.value}Option"),
                          p"Some(${Pat.Var(arg.paramName)})"
                        )
                      ),
                      arg.paramName
                    )
                  )
              },
        arg =>
          elemType =>
            _ =>
              if (arg.isFile) {
                Target.pure(Param(None, None, q"multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body)"))
              } else
                elemType match {
                  case t"String" =>
                    Target.pure(
                      Param(
                        Some(
                          enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid).sequence"
                        ),
                        None,
                        arg.paramName
                      )
                    )
                  case tpe =>
                    Target.pure(
                      Param(
                        Some(
                          enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence"
                        ),
                        None,
                        arg.paramName
                      )
                    )
                },
        arg =>
          elemType =>
            _ =>
              if (arg.isFile) {
                Target.pure(Param(None, None, q"Option(multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body)).filter(_.nonEmpty)"))
              } else
                elemType match {
                  case t"String" =>
                    Target.pure(
                      Param(
                        Some(
                          enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid).sequence.map(Option(_).filter(_.nonEmpty))"
                        ),
                        None,
                        arg.paramName
                      )
                    )
                  case tpe =>
                    Target.pure(
                      Param(
                        Some(
                          enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence.map(Option(_).filter(_.nonEmpty))"
                        ),
                        None,
                        arg.paramName
                      )
                    )
                },
        arg =>
          elemType =>
            if (arg.isFile) {
              Target.pure(Param(None, None, q"multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body)"))
            } else
              elemType match {
                case t"String" =>
                  Target.pure(
                    Param(
                      Some(
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid).sequence"
                      ),
                      None,
                      arg.paramName
                    )
                  )
                case tpe =>
                  Target.pure(
                    Param(
                      Some(
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence"
                      ),
                      None,
                      arg.paramName
                    )
                  )
              }
      )

    case class RenderedRoute(methodName: String, route: Case, methodSig: Decl.Def, supportDefinitions: List[Defn], handlerDefinitions: List[Stat])

    def generateRoute(
        resourceName: String,
        basePath: Option[String],
        methodName: String,
        responseClsName: String,
        route: RouteMeta,
        customExtractionFields: Option[CustomExtractionField[ScalaLanguage]],
        tracingFields: Option[TracingField[ScalaLanguage]],
        parameters: LanguageParameters[ScalaLanguage],
        responses: Responses[ScalaLanguage]
    ): Target[Option[RenderedRoute]] =
      // Generate the pair of the Handler method and the actual call to `complete(...)`
      Target.log.function("generateRoute")(for {
        _ <- Target.log.debug(s"Args: ${resourceName}, ${basePath}, ${route}, ${tracingFields}")
        RouteMeta(path, method, operation, securityRequirements) = route

        formArgs   <- prepareParameters(parameters.formParams)
        headerArgs <- prepareParameters(parameters.headerParams)
        pathArgs   <- prepareParameters(parameters.pathParams)
        qsArgs     <- prepareParameters(parameters.queryStringParams)
        bodyArgs   <- prepareParameters(parameters.bodyParams)

        http4sMethod <- httpMethodToHttp4s(method)
        pathWithQs   <- pathStrToHttp4s(basePath, path, pathArgs)
        (http4sPath, additionalQs) = pathWithQs
        http4sQs   <- qsToHttp4s(methodName)(qsArgs)
        http4sBody <- bodyToHttp4s(methodName, bodyArgs)
        asyncFormProcessing = formArgs.exists(_.isFile)
        http4sForm         <- if (asyncFormProcessing) asyncFormToHttp4s(methodName)(formArgs) else formToHttp4s(formArgs)
        http4sHeaders      <- headersToHttp4s(headerArgs)
        supportDefinitions <- generateSupportDefinitions(route, parameters)
      } yield {
        val (responseCompanionTerm, responseCompanionType) =
          (Term.Name(responseClsName), Type.Name(responseClsName))
        val responseType = ServerRawResponse(operation)
          .filter(_ == true)
          .fold[Type](t"$responseCompanionType")(Function.const(t"Response[F]"))
        val orderedParameters: List[List[LanguageParameter[ScalaLanguage]]] = List((pathArgs ++ qsArgs ++ bodyArgs ++ formArgs ++ headerArgs).toList) ++
              tracingFields
                .map(_.param)
                .map(List(_)) ++
              customExtractionFields
                .map(_.param)
                .map(List(_))

        val entityProcessor = http4sBody
          .orElse(Some((content: Term) => q"req.decode[UrlForm] { urlForm => $content }").filter(_ => formArgs.nonEmpty && formArgs.forall(!_.isFile)))
          .orElse(Some((content: Term) => q"req.decode[Multipart[F]] { multipart => $content }").filter(_ => formArgs.nonEmpty))
        val fullRouteMatcher =
          NonEmptyList.fromList(List(additionalQs, http4sQs).flatten).fold(p"$http4sMethod -> $http4sPath") { qs =>
            p"$http4sMethod -> $http4sPath :? ${qs.reduceLeft((a, n) => p"$a :& $n")}"
          }
        val fullRouteWithTracingMatcher = tracingFields
          .map(_ => p"$fullRouteMatcher ${Term.Name(s"usingFor${methodName.capitalize}")}(traceBuilder)")
          .getOrElse(fullRouteMatcher)
        val fullRouteWithTracingAndExtraction = customExtractionFields
          .map(_ => p"$fullRouteWithTracingMatcher ${Term.Name(s"extractorFor${methodName.capitalize}")}(extracted)")
          .getOrElse(fullRouteWithTracingMatcher)
        val handlerCallArgs: List[List[Term]] = List(List(responseCompanionTerm)) ++ List(
                (pathArgs ++ qsArgs ++ bodyArgs).map(_.paramName) ++ (http4sForm ++ http4sHeaders).map(_.handlerCallArg)
              ) ++
              tracingFields.map(_.param.paramName).map(List(_)) ++
              customExtractionFields.map(_.param.paramName).map(List(_))
        val handlerCall = q"handler.${Term.Name(methodName)}(...${handlerCallArgs})"
        val isGeneric   = Http4sHelper.isDefinitionGeneric(responses)
        val responseExpr = ServerRawResponse(operation)
          .filter(_ == true)
          .fold[Term] {
            val marshallers = responses.value.map {
              case Response(statusCodeName, valueType, headers) =>
                val responseTerm  = Term.Name(s"${statusCodeName.value}")
                val baseRespType  = Type.Select(responseCompanionTerm, Type.Name(statusCodeName.value))
                val respType      = if (isGeneric) Type.Apply(baseRespType, List(t"F")) else baseRespType
                val generatorName = Term.Name(s"$methodName${statusCodeName}EntityResponseGenerator")
                val encoderName   = Term.Name(s"$methodName${statusCodeName}Encoder")
                (valueType, headers.value) match {
                  case (None, Nil) =>
                    p"case $responseCompanionTerm.$responseTerm => F.pure(Response[F](status = org.http4s.Status.${statusCodeName}))"
                  case (Some(_), Nil) =>
                    p"case resp: $respType => $generatorName(resp.value)(F,$encoderName)"
                  case (None, headersList) =>
                    val (http4sHeaders, http4sHeadersDefinitions) = createHttp4sHeaders(headersList)
                    p"""case resp: $respType =>
                          ..$http4sHeadersDefinitions
                          F.pure(Response[F](status = org.http4s.Status.$statusCodeName, headers = Headers($http4sHeaders)))
                      """
                  case (Some(_), headersList) =>
                    val (http4sHeaders, http4sHeadersDefinitions) = createHttp4sHeaders(headersList)
                    val valueTerm                                 = q"resp.value"
                    p"""case resp: $respType =>
                          ..$http4sHeadersDefinitions
                          $generatorName($valueTerm, $http4sHeaders:_*)(F,$encoderName)
                      """
                }
            }
            q"$handlerCall flatMap ${Term.PartialFunction(marshallers)}"
          }(_ => handlerCall)
        val matchers = (http4sForm ++ http4sHeaders).flatMap(_.matcher)
        val responseInMatch = NonEmptyList.fromList(matchers).fold(responseExpr) {
          case NonEmptyList((expr, pat), Nil) =>
            Term.Match(expr, List(Case(pat, None, responseExpr), Case(p"_", None, q"""BadRequest("Invalid data")""")))
          case matchers @ NonEmptyList(_, _) =>
            val NonEmptyList(head, xs) = matchers.reverse
            val (base, rest)           = xs.splitAt(21).bimap(left => NonEmptyList(head, left).reverse, _.grouped(21).map(_.reverse.unzip).toList)
            val (buildTerms, buildPat) = rest.foldLeft[(Term => Term, Pat => Pat)]((identity, identity)) {
              case ((accTerm, accPat), (nextTermGroup, nextPatGroup)) =>
                (next => accTerm(q"(..${nextTermGroup :+ next})"), next => accPat(p"(..${nextPatGroup :+ next})"))
            }

            val (fullTerm, fullPat) = base match {
              case NonEmptyList((term, pat), Nil) =>
                (buildTerms(term), buildPat(pat))
              case NonEmptyList((term, pat), xs) =>
                val (terms, pats) = xs.unzip
                (buildTerms(q"(..${term +: terms})"), buildPat(p"(..${pat +: pats})"))
            }
            Term.Match(
              fullTerm,
              List(
                Case(fullPat, None, responseExpr),
                Case(Pat.Wildcard(), None, q"""BadRequest("Invalid data")""")
              )
            )
        }
        val responseInMatchInFor = (http4sForm ++ http4sHeaders).flatMap(_.generator) match {
          case Nil        => responseInMatch
          case generators => q"for {..${generators :+ enumerator"response <- $responseInMatch"}} yield response"
        }
        val routeBody = entityProcessor.fold[Term](responseInMatchInFor)(_.apply(responseInMatchInFor))

        val fullRoute: Case =
          p"""case req @ $fullRouteWithTracingAndExtraction =>
             mapRoute($methodName, req, {$routeBody})
            """

        val respond: List[List[Term.Param]] = List(List(param"respond: $responseCompanionTerm.type"))

        val params: List[List[Term.Param]] = respond ++ orderedParameters.map(
                _.map(
                  scalaParam =>
                    scalaParam.param.copy(
                      decltpe =
                        (
                          if (scalaParam.isFile) {
                            if (scalaParam.required) {
                              Some(t"Stream[F, Byte]")
                            } else {
                              Some(t"Option[Stream[F, Byte]]")
                            }
                          } else {
                            scalaParam.param.decltpe
                          }
                        )
                    )
                )
              )

        val consumes = operation.unwrapTracker.consumes.toList.flatMap(ContentType.unapply(_))
        val produces = operation.unwrapTracker.produces.toList.flatMap(ContentType.unapply(_))
        val codecs   = if (ServerRawResponse(operation).getOrElse(false)) Nil else generateCodecs(methodName, bodyArgs, responses, consumes, produces)
        val respType = if (isGeneric) t"$responseType[F]" else responseType
        Some(
          RenderedRoute(
            methodName,
            fullRoute,
            q"""def ${Term.Name(methodName)}(...${params}): F[$respType]""",
            supportDefinitions ++ generateQueryParamMatchers(methodName, qsArgs) ++ codecs ++
                tracingFields
                  .map(_.term)
                  .map(generateTracingExtractor(methodName, _)) ++
                customExtractionFields
                  .map(_.term)
                  .map(generateCustomExtractionFieldsExtractor(methodName, _)),
            List.empty //handlerDefinitions
          )
        )
      })

    def createHttp4sHeaders(headers: List[Header[ScalaLanguage]]): (Term.Name, List[Defn.Val]) = {
      val (names, definitions) = headers.map {
        case Header(name, required, _, termName) =>
          val nameLiteral = Lit.String(name)
          val headerName  = Term.Name(s"${name.toCamelCase}Header")
          val pattern     = Pat.Var(headerName)
          val v           = if (required) q"List(Header($nameLiteral, resp.$termName))" else q"resp.$termName.map(Header($nameLiteral,_)).toList"
          (headerName, q"val $pattern = $v")
      }.unzip
      val headersTerm = Term.Name("responseHeaders")
      val allHeaders  = q"val ${Pat.Var(headersTerm)} = List(..$names).flatten"
      (headersTerm, definitions :+ allHeaders)
    }

    def combineRouteTerms(terms: List[Case]): Target[Term] =
      Target.log.function("combineRouteTerms")(for {
        _      <- Target.log.debug(s"Args: <${terms.length} routes>")
        routes <- Target.fromOption(NonEmptyList.fromList(terms), UserError("Generated no routes, no source to generate"))
        _      <- routes.traverse(route => Target.log.debug(route.toString))
      } yield scala.meta.Term.PartialFunction(routes.toList))

    def generateSupportDefinitions(route: RouteMeta, parameters: LanguageParameters[ScalaLanguage]): Target[List[Defn]] =
      for {
        operation <- Target.pure(route.operation)

        pathArgs = parameters.pathParams
      } yield {
        generatePathParamExtractors(pathArgs)
      }

    def generatePathParamExtractors(pathArgs: List[LanguageParameter[ScalaLanguage]]): List[Defn] =
      pathArgs
        .map(_.argType)
        .flatMap({
          // Strip out provided type extractors (see dsl/src/main/scala/org/http4s/dsl/impl/Path.scala)
          case t"Int"            => None
          case t"Long"           => None
          case t"String"         => None
          case t"java.util.UUID" => None
          // Attempt to provide useful extractor names
          case tpe @ Type.Name(name)                 => Some(name -> tpe)
          case tpe @ Type.Select(_, Type.Name(name)) => Some(name -> tpe)
          // Give up for non-primitive type and rely on backtick-escaping
          case tpe => Some(tpe.toString -> tpe)
        })
        .distinctBy(_._1)
        .map({
          case (name, tpe) =>
            q"""
            object ${Term.Name(s"${name}Var")} {
              def unapply(str: String): Option[${tpe}] = {
                if (!str.isEmpty)
                  Json.fromString(str).as[${tpe}].toOption
                else
                  None
              }
            }
          """
        })

    def generateQueryParamMatchers(methodName: String, qsArgs: List[LanguageParameter[ScalaLanguage]]): List[Defn] = {
      val (decoders, matchers) = qsArgs
        .traverse({
          case LanguageParameter(_, param, _, argName, argType) =>
            val containerTransformations = Map[String, Term => Term](
              "Iterable"   -> identity _,
              "List"       -> (term => q"$term.toList"),
              "Vector"     -> (term => q"$term.toVector"),
              "Seq"        -> (term => q"$term.toSeq"),
              "IndexedSeq" -> (term => q"$term.toIndexedSeq")
            )
            val matcherName = Term.Name(s"${methodName.capitalize}${argName.value.capitalize}Matcher")
            val (queryParamMatcher, elemType) = param match {
              case param"$_: Option[$container[$tpe]]" if containerTransformations.contains(container.syntax) =>
                (q"""
                  object ${matcherName} {
                    val delegate = new OptionalMultiQueryParamDecoderMatcher[$tpe](${argName.toLit}) {}
                    def unapply(params: Map[String, Seq[String]]): Option[Option[$container[$tpe]]] = delegate.unapply(params).collectFirst {
                      case cats.data.Validated.Valid(value) => Option(value).filter(_.nonEmpty).map(x => ${containerTransformations(container.syntax)(q"x")})
                    }
                  }
                 """, tpe)
              case param"$_: Option[$container[$tpe]] = $_" if containerTransformations.contains(container.syntax) =>
                (q"""
                  object ${matcherName} {
                    val delegate = new OptionalMultiQueryParamDecoderMatcher[$tpe](${argName.toLit}) {}
                    def unapply(params: Map[String, Seq[String]]): Option[Option[$container[$tpe]]] = delegate.unapply(params).collectFirst {
                      case cats.data.Validated.Valid(value) => Option(value).filter(_.nonEmpty).map(x => ${containerTransformations(container.syntax)(q"x")})
                    }
                  }
                 """, tpe)
              case param"$_: Option[$tpe]" =>
                (
                  q"""object ${matcherName} extends OptionalQueryParamDecoderMatcher[$tpe](${argName.toLit})""",
                  tpe
                )
              case param"$_: Option[$tpe] = $_" =>
                (
                  q"""object ${matcherName} extends OptionalQueryParamDecoderMatcher[$tpe](${argName.toLit})""",
                  tpe
                )
              case param"$_: $container[$tpe]" if containerTransformations.contains(container.syntax) =>
                (q"""
                   object ${matcherName} {
                     val delegate = new QueryParamDecoderMatcher[$tpe](${argName.toLit}) {}
                     def unapply(params: Map[String, Seq[String]]): Option[$container[$tpe]] = delegate.unapplySeq(params).map(x => ${containerTransformations(
                  container.syntax
                )(q"x")})
                   }
                 """, tpe)
              case param"$_: $container[$tpe] = $_" if containerTransformations.contains(container.syntax) =>
                (q"""
                   object ${matcherName} {
                     val delegate = new QueryParamDecoderMatcher[$tpe](${argName.toLit}) {}
                     def unapply(params: Map[String, Seq[String]]): Option[$container[$tpe]] = delegate.unapplySeq(params).map(x => ${containerTransformations(
                  container.syntax
                )(q"x")})
                   }
                 """, tpe)
              case _ =>
                (
                  q"""object ${matcherName} extends QueryParamDecoderMatcher[$argType](${argName.toLit})""",
                  argType
                )
            }
            if (!List("Unit", "Boolean", "Double", "Float", "Short", "Int", "Long", "Char", "String").contains(elemType.toString())) {
              val queryParamDecoder = q"""
                implicit val ${Pat.Var(Term.Name(s"${elemType.toString()}QueryParamDecoder"))}: QueryParamDecoder[$elemType] = (value: QueryParameterValue) =>
                    Json.fromString(value.value).as[$elemType]
                      .leftMap(t => ParseFailure("Query decoding failed", t.getMessage))
                      .toValidatedNel
              """
              (List((elemType, queryParamDecoder)), queryParamMatcher)
            } else {
              (List.empty, queryParamMatcher)
            }
        })

      decoders.distinctBy(_._1.toString()).map(_._2) ++ matchers
    }

    /**
      * It's not possible to use backticks inside pattern matching as it has different semantics: backticks inside match
      * are just references to an already existing bindings.
      */
    def prepareParameters[F[_]: Traverse](parameters: F[LanguageParameter[ScalaLanguage]]): Target[F[LanguageParameter[ScalaLanguage]]] =
      if (parameters.exists(param => param.paramName.syntax != param.paramName.value)) {
        // let's try to prefix them all with underscore and see if it helps
        for {
          _ <- Target.log.debug("Found that not all parameters could be represented as unescaped terms")
          res <- parameters.traverse[Target, LanguageParameter[ScalaLanguage]] { param =>
            val newName = Term.Name(s"_${param.paramName.value}")
            Target.log.debug(s"Escaping param ${param.argName.value}").flatMap { _ =>
              if (newName.syntax == newName.value) Target.pure(param.withParamName(newName))
              else Target.raiseUserError(s"Can't escape parameter with name ${param.argName.value}.")
            }
          }
        } yield res
      } else {
        Target.pure(parameters)
      }

    def generateCodecs(
        methodName: String,
        bodyArgs: Option[LanguageParameter[ScalaLanguage]],
        responses: Responses[ScalaLanguage],
        consumes: Seq[ContentType],
        produces: Seq[ContentType]
    ): List[Defn.Val] =
      generateDecoders(methodName, bodyArgs, consumes) ++ generateEncoders(methodName, responses, produces) ++ generateResponseGenerators(
            methodName,
            responses
          )

    def generateDecoders(methodName: String, bodyArgs: Option[LanguageParameter[ScalaLanguage]], consumes: Seq[ContentType]): List[Defn.Val] =
      bodyArgs.toList.flatMap {
        case LanguageParameter(_, _, _, _, argType) =>
          List(
            q"private[this] val ${Pat.Typed(Pat.Var(Term.Name(s"${methodName.uncapitalized}Decoder")), t"EntityDecoder[F, $argType]")} = ${Http4sHelper
              .generateDecoder(argType, consumes)}"
          )
      }

    def generateEncoders(methodName: String, responses: Responses[ScalaLanguage], produces: Seq[ContentType]): List[Defn.Val] =
      for {
        response    <- responses.value
        (_, tpe, _) <- response.value
      } yield {
        val contentTypes = response.value.map(_._1).map(List(_)).getOrElse(produces) //for OpenAPI 3.x we should take ContentType from the response
        q"private[this] val ${Pat.Var(Term.Name(s"$methodName${response.statusCodeName}Encoder"))} = ${Http4sHelper.generateEncoder(tpe, contentTypes)}"
      }

    def generateResponseGenerators(methodName: String, responses: Responses[ScalaLanguage]): List[Defn.Val] =
      for {
        response <- responses.value
        if response.value.nonEmpty
      } yield {
        q"private[this] val ${Pat.Var(Term.Name(s"$methodName${response.statusCodeName}EntityResponseGenerator"))} = ${Http4sHelper
          .generateEntityResponseGenerator(q"org.http4s.Status.${response.statusCodeName}")}"
      }

    def generateTracingExtractor(methodName: String, tracingField: Term): Defn.Object =
      q"""
         object ${Term.Name(s"usingFor${methodName.capitalize}")} {
           def unapply(r: Request[F]): Option[(Request[F], TraceBuilder[F])] = Some(r -> $tracingField(r))
         }
       """

    def generateCustomExtractionFieldsExtractor(methodName: String, extractField: Term): Defn.Object =
      q"""
         object ${Term.Name(s"extractorFor${methodName.capitalize}")} {
           def unapply(r: Request[F]): Option[(Request[F], $customExtractionTypeName)] = Some(r -> $extractField(r))
         }
       """
  }
}
