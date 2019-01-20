package com.twilio.guardrail
package generators

import java.util.Locale

import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.shims._

import scala.collection.JavaConverters._
import scala.meta._

object EndpointsClientGenerator {
  object ClientTermInterp extends FunctionK[ClientTerm[ScalaLanguage, ?], Target] {

    private[this] def toDashedCase(s: String): String = {
      val lowercased =
        "^([A-Z])".r.replaceAllIn(s, m => m.group(1).toLowerCase(Locale.US))
      "([A-Z])".r
        .replaceAllIn(lowercased, m => '-' +: m.group(1).toLowerCase(Locale.US))
    }

    def apply[T](term: ClientTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(className, route @ RouteMeta(pathStr, httpMethod, operation), methodName, tracing, parameters, responses) =>
        def generateFormDataParams(parameters: List[ScalaParameter[ScalaLanguage]], needsMultipart: Boolean): Target[Option[Term]] =
          if (parameters.isEmpty) {
            Target.pure(None)
          } else if (needsMultipart) {
            Target.raiseError("Multipart forms are currently not supported in the endpoints generator")
          } else {
            def liftTerm(tParamName: Term, tName: RawParameterName) =
              q"List((${tName.toLit}, Formatter.show($tParamName)))"

            def liftIterable(tParamName: Term, tName: RawParameterName) =
              q"$tParamName.toList.map((${tName.toLit}, _))"

            def liftOptionTerm(tpe: Type)(tParamName: Term, tName: RawParameterName) = {
              val lifter = tpe match {
                case t"Iterable[$_]" => liftIterable _
                case _               => liftTerm _
              }
              q"${tParamName}.toList.flatMap(${Term.Block(List(q" x => ${lifter(Term.Name("x"), tName)}"))})"
            }

            val args: List[Term] = parameters.foldLeft(List.empty[Term]) {
              case (a, ScalaParameter(_, param, paramName, argName, _)) =>
                val lifter: (Term.Name, RawParameterName) => Term =
                  param match {
                    case param"$_: Option[$tpe]"      => liftOptionTerm(tpe) _
                    case param"$_: Option[$tpe] = $_" => liftOptionTerm(tpe) _
                    case _                            => liftTerm _
                  }
                a :+ lifter(paramName, argName)
            }
            Target.pure(Some(q"List(..$args).flatten"))
          }

        def generateHeaderParams(parameters: List[ScalaParameter[ScalaLanguage]]): Term = {
          def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
            q"$tParamName.map(v => RawHeader(${tName.toLit}, Formatter.show(v)))"

          def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
            q"Some(RawHeader(${tName.toLit}, Formatter.show($tParamName)))"

          val args: List[Term] = parameters.foldLeft(List.empty[Term]) {
            case (a, ScalaParameter(_, param, paramName, argName, _)) =>
              val lifter: (Term.Name, RawParameterName) => Term = param match {
                case param"$_: Option[$_]"      => liftOptionTerm _
                case param"$_: Option[$_] = $_" => liftOptionTerm _
                case _                          => liftTerm _
              }
              a :+ lifter(paramName, argName)
          }
          q"scala.collection.immutable.Seq[Option[String]](..$args).flatten"
        }

        def build(methodName: String,
                  httpMethod: HttpMethod,
                  pathPattern: Term,
                  staticQueryParams: Option[Term],
                  headerParams: Term,
                  responses: Responses[ScalaLanguage],
                  produces: Seq[RouteMeta.ContentType],
                  consumes: Seq[RouteMeta.ContentType],
                  tracing: Boolean)(tracingArgsPre: List[ScalaParameter[ScalaLanguage]],
                                    tracingArgsPost: List[ScalaParameter[ScalaLanguage]],
                                    pathArgs: List[ScalaParameter[ScalaLanguage]],
                                    qsArgs: List[ScalaParameter[ScalaLanguage]],
                                    formArgs: List[ScalaParameter[ScalaLanguage]],
                                    bodyArgs: Option[ScalaParameter[ScalaLanguage]],
                                    headerArgs: List[ScalaParameter[ScalaLanguage]],
                                    extraImplicits: List[Term.Param]): RenderedClientOperation[ScalaLanguage] = {

          val implicitParams = Option(extraImplicits).filter(_.nonEmpty)
          val defaultHeaders = param"headers: List[String] = Nil"

          val (fallbackBodyAlgebra, fallbackBodyArgument) =
            if (Set(HttpMethod.PUT, HttpMethod.POST) contains httpMethod)
              (Some(q"emptyRequest"), None)
            else (None, None)

          val (textPlainAlgebra, textPlainArgument): (Option[Term], Option[Term]) =
            if (consumes.contains(RouteMeta.TextPlain))
              (bodyArgs.map(_ => q"textPlainRequest"), bodyArgs.map(sp => if (sp.required) sp.paramName else q"""${sp.paramName}.getOrElse("")"""))
            else (None, None)

          val (formAlgebra, formArgument): (Option[Term], Option[Term]) = (None, None)

          val (tracingExpr, httpClientName) =
            if (tracing)
              (List(q"""val tracingHttpClient = traceBuilder(s"$${clientName}:$${methodName}")(httpClient)"""), q"tracingHttpClient")
            else
              (List.empty, q"httpClient")

          val urlWithParams = NonEmptyList.fromList(qsArgs).fold(pathPattern) { xs =>
            val (addArgs, components) =
              xs.traverse(x => (List(x.argType), q"showQs[${x.argType}](${x.argName.toLit})")).map(_.reduceLeft[Term]((a, b) => q"$a & $b"))
            q" $pathPattern /? $components "
          }
          val headersExpr = List(q"val allHeaders = headers ++ $headerParams")

          val bodyAlgebra: Option[Term] = formAlgebra
            .orElse(textPlainAlgebra)
            .orElse(bodyArgs.map(x => q"jsonRequest[${x.argType}](None)"))
            .orElse(fallbackBodyAlgebra)

          val bodyArgument: Option[Term] = formArgument
            .orElse(textPlainArgument)
            .orElse(bodyArgs.map(x => x.paramName))
            .orElse(fallbackBodyArgument)

          val responseCompanionTerm = Term.Name(s"${methodName.capitalize}Response")
          val responseCompanionType = Type.Name(s"${methodName.capitalize}Response")

          val cases = responses.value.map { resp =>
            val responseTerm = Term.Name(s"${resp.statusCodeName.value}")
            resp.value.fold[Case](
              p"case ${Lit.Int(resp.statusCode)} => Right($responseCompanionTerm.$responseTerm)"
            ) {
              case (tpe, _) =>
                p"""case ${Lit.Int(resp.statusCode)} =>
                  parser.parse(xhr.responseText).flatMap(CirceDecoder[${tpe}].decodeJson _).map($responseCompanionTerm.$responseTerm.apply _)
                """
            }
          } :+ p"case _ => Left(new UnknownStatusException(xhr))"

          val responseTypeRef = Type.Name(s"${methodName.capitalize}Response")

          val methodParameters: List[List[Term.Param]] = List(
            Some(
              (tracingArgsPre.map(_.param) ++ pathArgs.map(_.param) ++ qsArgs
                .map(_.param) ++ formArgs.map(_.param) ++ bodyArgs
                .map(_.param) ++ headerArgs.map(_.param) ++ tracingArgsPost
                .map(_.param)) :+ defaultHeaders
            ),
            implicitParams
          ).flatten

          val endpointCallArgs = {
            def adaptNel: NonEmptyList[Term] => Term = {
              case NonEmptyList(a, Nil) => a
              case NonEmptyList(a, x :: xs) => emulateTupler(a, NonEmptyList(x, xs))
            }
            @scala.annotation.tailrec
            def emulateTupler(base: Term, xs: NonEmptyList[Term]): Term.Tuple = xs match {
              case NonEmptyList(x, Nil) =>
                base match {
                  case q"($a, $b)" => q"($a, $b, $x)"
                  case as => q"($as, $x)"
                }
              case NonEmptyList(x, y :: Nil) =>
                base match {
                  case q"($a, $b)" => q"($a, $b, $x, $y)"
                  case as => q"($as, $x, $y)"
                }
              case NonEmptyList(x, y :: rest) =>
                emulateTupler(base match {
                  case q"($a, $b)" => q"($a, $b, $x)"
                  case as => q"($as, $x)"
                }, NonEmptyList(y, rest))
            }

            def hackyFoldLimitedTupleTree: NonEmptyList[Term] => NonEmptyList[Term] = { case NonEmptyList(x, xs) =>
              // FIXME: There's a hard limit in endpoints for a maximum Tupler size of 3 elements.
              // This results in ((((1, 2, 3), 4, 5), 6, 7)...)
              // As a result, we've got a rather complex structure to replicate this,
              // bounded by ${limit}. If this limit is ever raised, it'll be a breaking change, unless
              // this value becomes customizable.
              val limit = 2
              val h     = xs.grouped(limit).take(1)
              val t     = xs.grouped(limit).drop(1)
              t.foldLeft[NonEmptyList[Term]](
                h.foldLeft[NonEmptyList[Term]](NonEmptyList(x, Nil)) {
                  case (a, x :: Nil) => a ++ List(x)
                  case (a, xs)       => NonEmptyList(q"(..${a.toList ++ xs.toList})", Nil)
                }
              ) {
                case (a, x :: Nil) => a ++ List(x)
                case (a, xs)       => NonEmptyList(q"(..${a.toList ++ xs.toList})", Nil)
              }
            }
            val hostPart: List[Term] = List(q"host", q"basePath")
            val pathPart: List[Term] = hostPart ++ pathArgs.map(_.paramName)
            val qsPart: List[Term] = qsArgs.map(_.paramName)
            val bodyPart: List[Term] = bodyArgument.toList
            val headerPart: List[Term] = headerArgs.map(_.paramName)

            val res0: Option[NonEmptyList[NonEmptyList[Term]]] =
              NonEmptyList.fromList(List[List[Term]](
                pathPart,
                qsPart,
                bodyPart,
                headerPart
              ).flatMap(NonEmptyList.fromList(_)))

            res0.fold[Term](q"()")({ nel =>
              val res@NonEmptyList(h, t) = nel.map(hackyFoldLimitedTupleTree)
              t.foldLeft(adaptNel(h))(emulateTupler(_, _))
            })
          }

          val algebraParams = List[Option[Term]](
            Some(urlWithParams),
            bodyAlgebra,
            NonEmptyList
              .fromList(headerArgs)
              .map(_.map[Term]({ arg =>
                arg.argType match {
                  case t"Option[$tpe]" => q"""showOptHeader[${tpe}](${arg.argName.toLit}, None)"""
                  case tpe             => q"""showHeader[${tpe}](${arg.argName.toLit}, None)"""
                }
              }).reduceLeft((a, b) => q"$a ++ $b"))
          ).flatten

          val endpointDefinition = q"""
              endpoint(${Term.Name(httpMethod.toString.toLowerCase)}(..${algebraParams}), ${Term.Name(s"${methodName}ResponseMapper")})
            """

          val methodBody = q"""
            EitherT(
              ${Term.Name(s"${methodName}Endpoint")}.apply(${endpointCallArgs})
                .transformWith[Either[Either[Throwable, XMLHttpRequest], $responseTypeRef]](
                  x => Future.successful(Right(x)),
                  { case UnknownStatusException(xhr) => Future.successful(Left(Right(xhr)))
                    case ex => Future.successful(Left(Left(ex)))
                  }))
          """

          RenderedClientOperation[ScalaLanguage](
            q"""
              def ${Term.Name(methodName)}(...${methodParameters}): EitherT[Result, Either[Throwable, XMLHttpRequest], $responseTypeRef] = $methodBody
            """,
            List(
              q"""
              def ${Term.Name(s"${methodName}ResponseMapper")}: Response[$responseCompanionType] = xhr => ${Term
                .Match(q"xhr.status", cases)}
              """,
              q"""
                val ${Pat.Var(Term.Name(s"${methodName}Endpoint"))} = $endpointDefinition
              """
            )
          )
        }

        for {
          // Placeholder for when more functions get logging
          _ <- Target.pure(())

          produces = operation.produces.toList.flatMap(RouteMeta.ContentType.unapply(_))
          consumes = operation.consumes.toList.flatMap(RouteMeta.ContentType.unapply(_))

          headerArgs = parameters.headerParams
          pathArgs   = parameters.pathParams
          qsArgs     = parameters.queryStringParams
          bodyArgs   = parameters.bodyParams
          formArgs   = parameters.formParams

          _ <- Target.log.debug("generateClientOperation")(s"pathArgs: $pathArgs")

          // Generate the url with path, query parameters
          urlWithPathParams <- SwaggerUtil.paths.generateUrlEndpointsPathExtractors(pathStr, pathArgs)
          (pathPattern, staticQueryParams) = urlWithPathParams

          // _ <- Target.log.debug("generateClientOperation")(s"Generated: $urlWithParams")
          // Generate FormData arguments
          formDataParams <- generateFormDataParams(formArgs, consumes.contains(RouteMeta.MultipartFormData))
          // Generate header arguments
          headerParams = generateHeaderParams(headerArgs)

          tracingArgsPre = if (tracing)
            List(ScalaParameter.fromParam(param"traceBuilder: TraceBuilder"))
          else List.empty
          tracingArgsPost = if (tracing)
            List(ScalaParameter.fromParam(param"methodName: String = ${Lit.String(toDashedCase(methodName))}"))
          else List.empty
          extraImplicits = List.empty
          renderedClientOperation = build(methodName, httpMethod, pathPattern, staticQueryParams, headerParams, responses, produces, consumes, tracing)(
            tracingArgsPre,
            tracingArgsPost,
            pathArgs,
            qsArgs,
            formArgs,
            bodyArgs,
            headerArgs,
            extraImplicits
          )
        } yield renderedClientOperation
      case GetImports(tracing)                             => Target.pure(List.empty)
      case GetExtraImports(tracing)                        => Target.pure(List.empty)
      case ClientClsArgs(tracingName, serverUrls, tracing) => Target.raiseError("Client generation impossible, as constructor has not been defined")
      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        Target.pure(Http4sHelper.generateResponseDefinitions(operationId, responses, protocolElems))
      case BuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing) => Target.raiseError("Client generation impossible, as constructor has not been defined")
      case BuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing) =>
        val (endpointDefs, rest0) = supportDefinitions.partition {
          case q"val $name = endpoint(...$_)" => true
          case _                              => false
        }
        val (responseDefs, rest1) = rest0.partition {
          case q"def $name: Response[$tpe] = $_" => true
          case _                                 => false
        }
        val algebra =
          q"""
            trait ${Type.Name(s"${clientName}Algebra")} extends algebra.Endpoints with algebra.circe.JsonEntitiesFromCodec with AddPathSegments with FormData {
              ..${responseDefs.map {
            case q"def $name: Response[$tpe] = $_" => q"def $name: Response[$tpe]"
          }};
              ..${endpointDefs}
            }
         """
        val client =
          q"""
            class ${Type
            .Name(clientName)}(...$ctorArgs) extends ${Init(Type.Name(s"${clientName}Algebra"), Name(""), Nil)} with xhr.JsonEntitiesFromCodec with xhr.faithful.Endpoints with XhrAddPathSegments with XhrFormData {
              ..$responseDefs;
              ..$rest1;
              private[this] def makeRequest[A](value: Future[A]): EitherT[Future, Either[Throwable, XMLHttpRequest], A] =
                EitherT(value.transformWith[Either[Either[Throwable, XMLHttpRequest], A]](x => Future.successful(Right(x)), {
                  case UnknownStatusException(xhr) =>
                    Future.successful(Left(Right(xhr)))
                  case ex =>
                    Future.successful(Left(Left(ex)))
                }));
              ..$clientCalls;
            }
          """
        Target.pure(NonEmptyList(Left(algebra), Right(client) :: Nil))
    }
  }
}
