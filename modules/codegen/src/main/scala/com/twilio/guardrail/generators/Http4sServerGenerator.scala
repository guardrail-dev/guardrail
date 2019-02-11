package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.extract.{ ScalaPackage, ScalaTracingLabel, ServerRawResponse }
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.RouteMeta
import scala.collection.JavaConverters._
import scala.meta.{ Term, _ }
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod

object Http4sServerGenerator {
  object ServerTermInterp extends FunctionK[ServerTerm[ScalaLanguage, ?], Target] {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }
    def apply[T](term: ServerTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        Target.pure(Http4sHelper.generateResponseDefinitions(operationId, responses, protocolElems))

      case BuildTracingFields(operation, resourceName, tracing) =>
        for {
          _ <- Target.log.debug("Http4sServerGenerator", "server")(s"buildTracingFields(${operation}, ${resourceName}, ${tracing})")
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
            } yield Some(TracingField[ScalaLanguage](ScalaParameter.fromParam(param"traceBuilder: TraceBuilder[F]"), q"""trace(${label})"""))
          } else Target.pure(None)
        } yield res

      case GenerateRoutes(resourceName, basePath, routes, protocolElems) =>
        for {
          renderedRoutes <- routes
            .traverse {
              case (operationId, tracingFields, sr @ RouteMeta(path, method, operation), parameters, responses) =>
                generateRoute(resourceName, basePath, sr, tracingFields, parameters, responses)
            }
            .map(_.flatten)
          routeTerms = renderedRoutes.map(_.route)
          combinedRouteTerms <- combineRouteTerms(routeTerms)
          methodSigs = renderedRoutes.map(_.methodSig)
        } yield {
          RenderedRoutes[ScalaLanguage](
            combinedRouteTerms,
            methodSigs,
            renderedRoutes.flatMap(_.supportDefinitions),
            renderedRoutes.flatMap(_.handlerDefinitions)
          )
        }

      case RenderHandler(handlerName, methodSigs, handlerDefinitions) =>
        for {
          _ <- Target.log.debug("Http4sServerGenerator", "server")(s"renderHandler(${handlerName}, ${methodSigs}")
        } yield q"""
          trait ${Type.Name(handlerName)}[F[_]] {
            ..${methodSigs ++ handlerDefinitions}
          }
        """

      case GetExtraRouteParams(tracing) =>
        for {
          _ <- Target.log.debug("Http4sServerGenerator", "server")(s"getExtraRouteParams(${tracing})")
          res <- if (tracing) {
            Target.pure(List(param"""trace: String => Request[F] => TraceBuilder[F]"""))
          } else Target.pure(List.empty)
        } yield res

      case RenderClass(resourceName, handlerName, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        for {
          _ <- Target.log.debug("Http4sServerGenerator", "server")(s"renderClass(${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams})")
          routesParams = List(param"handler: ${Type.Name(handlerName)}[F]")
        } yield q"""
          class ${Type.Name(resourceName)}[F[_]](..$extraRouteParams)(implicit E: Effect[F]) extends Http4sDsl[F] {

            ..${supportDefinitions};
            def routes(..${routesParams}): HttpRoutes[F] = HttpRoutes.of {
              ${combinedRouteTerms}
            }
          }
        """ +: responseDefinitions

      case GetExtraImports(tracing) =>
        for {
          _ <- Target.log.debug("Http4sServerGenerator", "server")(s"getExtraImports(${tracing})")
        } yield
          List(
            q"import org.http4s.dsl.Http4sDsl",
            q"import fs2.text._"
          )
    }

    def httpMethodToHttp4s(method: HttpMethod): Target[Term.Name] = method match {
      case HttpMethod.DELETE => Target.pure(Term.Name("DELETE"))
      case HttpMethod.GET    => Target.pure(Term.Name("GET"))
      case HttpMethod.PATCH  => Target.pure(Term.Name("PATCH"))
      case HttpMethod.POST   => Target.pure(Term.Name("POST"))
      case HttpMethod.PUT    => Target.pure(Term.Name("PUT"))
      case other             => Target.raiseError(s"Unknown method: ${other}")
    }

    def pathStrToHttp4s(basePath: Option[String], path: String, pathArgs: List[ScalaParameter[ScalaLanguage]]): Target[(Pat, Option[Pat])] =
      (basePath.getOrElse("") + path).stripPrefix("/") match {
        case "" => Target.pure((p"${Term.Name("Root")}", None))
        case path =>
          for {
            pathDirective <- SwaggerUtil.paths
              .generateUrlHttp4sPathExtractors(path, pathArgs)
          } yield pathDirective
      }

    def directivesFromParams[T](
        required: ScalaParameter[ScalaLanguage] => Type => Target[T],
        multi: ScalaParameter[ScalaLanguage] => Type => Target[T],
        multiOpt: ScalaParameter[ScalaLanguage] => Type => Target[T],
        optional: ScalaParameter[ScalaLanguage] => Type => Target[T]
    )(params: List[ScalaParameter[ScalaLanguage]]): Target[List[T]] =
      for {
        directives <- params.traverse[Target, T] {
          case scalaParam @ ScalaParameter(_, param, _, _, argType) =>
            param match {
              case param"$_: Option[Iterable[$tpe]]" =>
                multiOpt(scalaParam)(tpe)
              case param"$_: Option[Iterable[$tpe]] = $_" =>
                multiOpt(scalaParam)(tpe)
              case param"$_: Option[$tpe]" =>
                optional(scalaParam)(tpe)
              case param"$_: Option[$tpe] = $_" =>
                optional(scalaParam)(tpe)
              case param"$_: Iterable[$tpe]" =>
                multi(scalaParam)(tpe)
              case param"$_: Iterable[$tpe] = $_" =>
                multi(scalaParam)(tpe)
              case _ => required(scalaParam)(argType)
            }
        }
      } yield directives

    def bodyToHttp4s(operationId: String, body: Option[ScalaParameter[ScalaLanguage]]): Target[Option[Term => Term]] =
      Target.pure(
        body.map {
          case ScalaParameter(_, _, paramName, _, _) =>
            content =>
              q"req.decodeWith(${Term.Name(s"${operationId}Decoder")}, strict = false) { ${param"$paramName"} => $content }"
        }
      )

    case class Param(generator: Option[Enumerator.Generator], matcher: Option[(Term, Pat)], handlerCallArg: Term)

    def headersToHttp4s: List[ScalaParameter[ScalaLanguage]] => Target[List[Param]] =
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
        arg => _ => Target.raiseError(s"Unsupported Iterable[${arg}"),
        arg => _ => Target.raiseError(s"Unsupported Option[Iterable[${arg}]]"),
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

    def qsToHttp4s(operationId: String): List[ScalaParameter[ScalaLanguage]] => Target[Option[Pat]] =
      params =>
        directivesFromParams(
          arg => _ => Target.pure(p"${Term.Name(s"${operationId.capitalize}${arg.paramName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)})"),
          arg => _ => Target.pure(p"${Term.Name(s"${operationId.capitalize}${arg.paramName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)} @ _*)"),
          arg => _ => Target.pure(p"${Term.Name(s"${operationId.capitalize}${arg.paramName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)})"),
          arg => _ => Target.pure(p"${Term.Name(s"${operationId.capitalize}${arg.paramName.value.capitalize}Matcher")}(${Pat.Var(arg.paramName)})")
        )(params).map {
          case Nil => Option.empty
          case x :: xs =>
            Some(xs.foldLeft[Pat](x) { case (a, n) => p"${a} +& ${n}" })
      }

    def formToHttp4s: List[ScalaParameter[ScalaLanguage]] => Target[List[Param]] =
      directivesFromParams(
        arg => {
          case t"String" =>
            Target.pure(Param(None, Some((q"urlForm.values.get(${arg.argName.toLit})", p"Some(Seq(${Pat.Var(arg.paramName)}))")), arg.paramName))
          case tpe =>
            Target.pure(
              Param(
                None,
                Some((q"urlForm.values.get(${arg.argName.toLit}).map(_.map(Json.fromString(_).as[$tpe]))", p"Some(Seq(Right(${Pat.Var(arg.paramName)})))")),
                arg.paramName
              )
            )
        },
        arg => {
          case t"String" => Target.pure(Param(None, Some((q"urlForm.values.get(${arg.argName.toLit})", p"Some(${Pat.Var(arg.paramName)})")), arg.paramName))
          case tpe =>
            Target.pure(
              Param(
                None,
                Some(
                  (
                    q"urlForm.values.get(${arg.argName.toLit}).map(_.map(Json.fromString(_).as[$tpe]).sequence)",
                    p"Some(Right(${Pat.Var(arg.paramName)}))"
                  )
                ),
                arg.paramName
              )
            )
        },
        arg => {
          case t"String" => Target.pure(Param(None, None, q"urlForm.values.get(${arg.argName.toLit})"))
          case tpe =>
            Target.pure(
              Param(
                None,
                Some(
                  (
                    q"urlForm.values.get(${arg.argName.toLit}).map(_.map(Json.fromString(_).as[$tpe]).sequence).sequence",
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

    def asyncFormToHttp4s(operationId: String): List[ScalaParameter[ScalaLanguage]] => Target[List[Param]] =
      directivesFromParams(
        arg =>
          elemType =>
            if (arg.isFile) {
              Target.pure(
                Param(None,
                      Some((q"multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body)", p"Some(${Pat.Var(arg.paramName)})")),
                      arg.paramName)
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
                        enumerator"${Pat.Var(Term.Name(s"${arg.argName.value}Option"))} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => E.fromEither(Json.fromString(str).as[$tpe]))).sequence"
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
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => E.fromEither(Json.fromString(str).as[$tpe]))).sequence"
                      ),
                      None,
                      arg.paramName
                    )
                  )
          },
        arg =>
          elemType =>
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
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => E.fromEither(Json.fromString(str).as[$tpe]))).sequence.map(Option(_).filter(_.nonEmpty))"
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
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through(utf8Decode).compile.foldMonoid.flatMap(str => E.fromEither(Json.fromString(str).as[$tpe]))).sequence"
                      ),
                      None,
                      arg.paramName
                    )
                  )
          }
      )

    case class RenderedRoute(route: Case, methodSig: Decl.Def, supportDefinitions: List[Defn], handlerDefinitions: List[Stat])

    def generateRoute(resourceName: String,
                      basePath: Option[String],
                      route: RouteMeta,
                      tracingFields: Option[TracingField[ScalaLanguage]],
                      parameters: ScalaParameters[ScalaLanguage],
                      responses: Responses[ScalaLanguage]): Target[Option[RenderedRoute]] =
      // Generate the pair of the Handler method and the actual call to `complete(...)`
      for {
        _ <- Target.log.debug("Http4sServerGenerator", "server")(s"generateRoute(${resourceName}, ${basePath}, ${route}, ${tracingFields})")
        RouteMeta(path, method, operation) = route
        operationId <- Target.fromOption(Option(operation.getOperationId())
                                           .map(splitOperationParts)
                                           .map(_._2),
                                         "Missing operationId")

        formArgs   = parameters.formParams
        headerArgs = parameters.headerParams
        pathArgs   = parameters.pathParams
        qsArgs     = parameters.queryStringParams
        bodyArgs   = parameters.bodyParams

        http4sMethod <- httpMethodToHttp4s(method)
        pathWithQs   <- pathStrToHttp4s(basePath, path, pathArgs)
        (http4sPath, additionalQs) = pathWithQs
        http4sQs   <- qsToHttp4s(operationId)(qsArgs)
        http4sBody <- bodyToHttp4s(operationId, bodyArgs)
        asyncFormProcessing = formArgs.exists(_.isFile)
        http4sForm         <- if (asyncFormProcessing) asyncFormToHttp4s(operationId)(formArgs) else formToHttp4s(formArgs)
        http4sHeaders      <- headersToHttp4s(headerArgs)
        supportDefinitions <- generateSupportDefinitions(route, parameters)
      } yield {
        val (responseCompanionTerm, responseCompanionType) =
          (Term.Name(s"${operationId.capitalize}Response"), Type.Name(s"${operationId.capitalize}Response"))
        val responseType = ServerRawResponse(operation)
          .filter(_ == true)
          .fold[Type](t"$responseCompanionType")(Function.const(t"Response[F]"))
        val orderedParameters
          : List[List[ScalaParameter[ScalaLanguage]]] = List((pathArgs ++ qsArgs ++ bodyArgs ++ formArgs ++ headerArgs).toList) ++ tracingFields
          .map(_.param)
          .map(List(_))

        val entityProcessor = http4sBody
          .orElse(Some((content: Term) => q"req.decode[UrlForm] { urlForm => $content }").filter(_ => formArgs.nonEmpty && formArgs.forall(!_.isFile)))
          .orElse(Some((content: Term) => q"req.decode[Multipart[F]] { multipart => $content }").filter(_ => formArgs.nonEmpty))
        val fullRouteMatcher =
          List(additionalQs, http4sQs).flatten match {
            case Nil => p"$http4sMethod -> $http4sPath"
            case qs  => p"$http4sMethod -> $http4sPath :? ${qs.reduceLeft((a, n) => p"$a :& $n")}"
          }
        val fullRouteWithTracingMatcher = tracingFields
          .map(_ => p"$fullRouteMatcher ${Term.Name(s"usingFor${operationId.capitalize}")}(traceBuilder)")
          .getOrElse(fullRouteMatcher)
        val handlerCallArgs: List[List[Term]] = List(List(responseCompanionTerm)) ++ List(
          (pathArgs ++ qsArgs ++ bodyArgs).map(_.paramName) ++ (http4sForm ++ http4sHeaders).map(_.handlerCallArg)
        ) ++ tracingFields.map(_.param.paramName).map(List(_))
        val handlerCall = q"handler.${Term.Name(operationId)}(...${handlerCallArgs})"
        val responseExpr = ServerRawResponse(operation)
          .filter(_ == true)
          .fold[Term] {
            val marshallers = responses.value.map {
              case Response(statusCodeName, valueType) =>
                val responseTerm = Term.Name(s"${statusCodeName.value}")
                valueType.fold[Case](
                  p"case $responseCompanionTerm.$responseTerm => ${statusCodeName}()"
                ) { _ =>
                  p"case $responseCompanionTerm.$responseTerm(value) => ${statusCodeName}(value)(E, ${Term.Name(s"$operationId${statusCodeName}Encoder")})"
                }
            }
            q"$handlerCall flatMap ${Term.PartialFunction(marshallers)}"
          }(_ => handlerCall)
        val matchers = (http4sForm ++ http4sHeaders).flatMap(_.matcher)
        val responseInMatch =
          matchers match {
            case Nil => responseExpr
            case (expr, pat) :: Nil =>
              Term.Match(expr, List(Case(pat, None, responseExpr), Case(p"_", None, q"""BadRequest("Invalid data")""")))
            case _ =>
              q"""
              (..${matchers.map(_._1)}) match {
                case (..${matchers.map(_._2)}) => $responseExpr
                case _ => BadRequest("Invalid data")
              }
              """
          }
        val responseInMatchInFor = (http4sForm ++ http4sHeaders).flatMap(_.generator) match {
          case Nil        => responseInMatch
          case generators => q"for {..${generators :+ enumerator"response <- $responseInMatch"}} yield response"
        }
        val routeBody       = entityProcessor.fold[Term](responseInMatchInFor)(_.apply(responseInMatchInFor))
        val fullRoute: Case = p"case req @ $fullRouteWithTracingMatcher => $routeBody"

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

        val consumes = operation.consumes
        val produces = operation.produces
        Some(
          RenderedRoute(
            fullRoute,
            q"""def ${Term.Name(operationId)}(...${params}): F[${responseType}]""",
            supportDefinitions ++ generateQueryParamMatchers(operationId, qsArgs) ++ generateCodecs(operationId, bodyArgs, responses, consumes, produces) ++ tracingFields
              .map(_.term)
              .map(generateTracingExtractor(operationId, _)),
            List.empty //handlerDefinitions
          )
        )
      }

    def combineRouteTerms(terms: List[Case]): Target[Term] =
      for {
        _      <- Target.log.debug("Http4sServerGenerator", "server")(s"combineRouteTerms(<${terms.length} routes>)")
        routes <- Target.fromOption(NonEmptyList.fromList(terms), "Generated no routes, no source to generate")
        _      <- routes.traverse(route => Target.log.debug("Http4sServerGenerator", "server", "combineRouteTerms")(route.toString))
      } yield scala.meta.Term.PartialFunction(routes.toList)

    def generateSupportDefinitions(route: RouteMeta, parameters: ScalaParameters[ScalaLanguage]): Target[List[Defn]] =
      for {
        operation <- Target.pure(route.operation)

        pathArgs = parameters.pathParams
      } yield {
        generatePathParamExtractors(pathArgs)
      }

    def generatePathParamExtractors(pathArgs: List[ScalaParameter[ScalaLanguage]]): List[Defn] =
      pathArgs
        .map(_.argType.toString)
        .distinct
        .filter(!List("Int", "Long", "String").contains(_))
        .map(tpe => q"""
          object ${Term.Name(s"${tpe}Var")} {
            def unapply(str: String): Option[${Type.Name(tpe)}] = {
              if (!str.isEmpty)
                Json.fromString(str).as[${Type.Name(tpe)}].toOption
              else
                None
            }
          }
        """)

    def generateQueryParamMatchers(operationId: String, qsArgs: List[ScalaParameter[ScalaLanguage]]): List[Defn] =
      qsArgs
        .flatMap {
          case ScalaParameter(_, param, _, argName, argType) =>
            val (queryParamMatcher, elemType) = param match {
              case param"$_: Option[Iterable[$tpe]]" =>
                (q"""object ${Term
                   .Name(s"${operationId.capitalize}${argName.value.capitalize}Matcher")} extends OptionalMultiQueryParamDecoderMatcher[$tpe](${argName.toLit})""",
                 tpe)
              case param"$_: Option[Iterable[$tpe]] = $_" =>
                (q"""object ${Term
                   .Name(s"${operationId.capitalize}${argName.value.capitalize}Matcher")} extends OptionalMultiQueryParamDecoderMatcher[$tpe](${argName.toLit})""",
                 tpe)
              case param"$_: Option[$tpe]" =>
                (q"""object ${Term
                   .Name(s"${operationId.capitalize}${argName.value.capitalize}Matcher")} extends OptionalQueryParamDecoderMatcher[$tpe](${argName.toLit})""",
                 tpe)
              case param"$_: Option[$tpe] = $_" =>
                (q"""object ${Term
                   .Name(s"${operationId.capitalize}${argName.value.capitalize}Matcher")} extends OptionalQueryParamDecoderMatcher[$tpe](${argName.toLit})""",
                 tpe)
              case param"$_: Iterable[$tpe]" =>
                (q"""
                   object ${Term.Name(s"${operationId.capitalize}${argName.value.capitalize}Matcher")} {
                     def unapplySeq(params: Map[String, Seq[String]]): Option[Seq[String]] = new QueryParamDecoderMatcher[$tpe](${argName.toLit}) {}.unapplySeq(params)
                   }
                 """, tpe)
              case param"$_: Iterable[$tpe] = $_" =>
                (q"""
                   object ${Term.Name(s"${operationId.capitalize}${argName.value.capitalize}Matcher")} {
                     def unapplySeq(params: Map[String, Seq[String]]): Option[Seq[String]] = new QueryParamDecoderMatcher[$tpe](${argName.toLit}) {}.unapplySeq(params)
                   }
                 """, tpe)
              case _ =>
                (q"""object ${Term
                   .Name(s"${operationId.capitalize}${argName.value.capitalize}Matcher")} extends QueryParamDecoderMatcher[$argType](${argName.toLit})""",
                 argType)
            }
            if (!List("Unit", "Boolean", "Double", "Float", "Short", "Int", "Long", "Char", "String").contains(elemType.toString())) {
              val queryParamDecoder = q"""
                implicit val ${Pat.Var(Term.Name(s"${argName.value}QueryParamDecoder"))}: QueryParamDecoder[$elemType] = (value: QueryParameterValue) =>
                    Json.fromString(value.value).as[$elemType]
                      .leftMap(t => ParseFailure("Query decoding failed", t.getMessage))
                      .toValidatedNel
              """
              List(queryParamMatcher, queryParamDecoder)
            } else {
              List(queryParamMatcher)
            }
        }

    def generateCodecs(operationId: String,
                       bodyArgs: Option[ScalaParameter[ScalaLanguage]],
                       responses: Responses[ScalaLanguage],
                       consumes: Seq[String],
                       produces: Seq[String]): List[Defn.Val] =
      generateDecoders(operationId, bodyArgs, consumes) ++ generateEncoders(operationId, responses, produces)

    def generateDecoders(operationId: String, bodyArgs: Option[ScalaParameter[ScalaLanguage]], consumes: Seq[String]): List[Defn.Val] =
      bodyArgs.toList.flatMap {
        case ScalaParameter(_, _, _, _, argType) =>
          List(
            q"val ${Pat.Typed(Pat.Var(Term.Name(s"${operationId}Decoder")), t"EntityDecoder[F, $argType]")} = ${Http4sHelper.generateDecoder(argType, consumes)}"
          )
      }

    def generateEncoders(operationId: String, responses: Responses[ScalaLanguage], produces: Seq[String]): List[Defn.Val] =
      for {
        response        <- responses.value
        typeDefaultPair <- response.value
        (tpe, _) = typeDefaultPair
      } yield {
        q"val ${Pat.Var(Term.Name(s"$operationId${response.statusCodeName}Encoder"))} = ${Http4sHelper.generateEncoder(tpe, produces)}"
      }

    def generateTracingExtractor(operationId: String, tracingField: Term) =
      q"""
         object ${Term.Name(s"usingFor${operationId.capitalize}")} {
           def unapply(r: Request[F]): Option[(Request[F], TraceBuilder[F])] = Some(r -> $tracingField(r))
         }
       """
  }
}
