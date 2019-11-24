package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.{ Header, Responses }
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.RouteMeta
import scala.meta._
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import java.net.URI

object Http4sClientGenerator {

  object ClientTermInterp extends FunctionK[ClientTerm[ScalaLanguage, ?], Target] {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }

    private[this] def formatClientName(clientName: Option[String]): Term.Param =
      clientName.fold(
        param"clientName: String"
      )(name => param"clientName: String = ${Lit.String(name.toDashedCase)}")

    private[this] def formatHost(serverUrls: Option[NonEmptyList[URI]]): Term.Param =
      serverUrls
        .fold(param"host: String")(v => param"host: String = ${Lit.String(v.head.toString())}")

    def apply[T](term: ClientTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(
          className,
          RouteMeta(pathStr, httpMethod, operation, securityRequirements),
          methodName,
          tracing,
          parameters,
          responses,
          securitySchemes
          ) =>
        def generateUrlWithParams(
            path: Tracker[String],
            pathArgs: List[ScalaParameter[ScalaLanguage]],
            qsArgs: List[ScalaParameter[ScalaLanguage]]
        ): Target[Term] =
          Target.log.function("generateUrlWithParams")(for {
            _    <- Target.log.debug(s"Using ${path.get} and ${pathArgs.map(_.argName)}")
            base <- generateUrlPathParams(path, pathArgs)

            _ <- Target.log.debug(s"QS: ${qsArgs}")

            suffix = if (path.unwrapTracker.contains("?")) {
              Lit.String("&")
            } else {
              Lit.String("?")
            }

            _ <- Target.log.debug(s"QS: ${qsArgs}")

            result = NonEmptyList
              .fromList(qsArgs.toList)
              .fold(base)({
                _.foldLeft[Term](q"${base} + ${suffix}") {
                  case (a, ScalaParameter(_, _, paramName, argName, _)) =>
                    q""" $a + Formatter.addArg(${Lit
                      .String(argName.value)}, ${paramName})"""
                }
              })
          } yield q"Uri.unsafeFromString(${result})")

        def generateFormDataParams(parameters: List[ScalaParameter[ScalaLanguage]], needsMultipart: Boolean): Option[Term] =
          if (parameters.isEmpty) {
            None
          } else if (needsMultipart) {
            def liftOptionFileTerm(tParamName: Term.Name, tName: RawParameterName) =
              q"$tParamName.map(v => Part.fileData[F](${tName.toLit}, v._1, v._2))"

            def liftFileTerm(tParamName: Term.Name, tName: RawParameterName) =
              q"Some(Part.fileData[F](${tName.toLit}, ${tParamName}._1, ${tParamName}._2))"

            def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
              q"$tParamName.map(v => Part.formData[F](${tName.toLit}, Formatter.show(v)))"

            def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
              q"Some(Part.formData[F](${tName.toLit}, Formatter.show($tParamName)))"

            val args: List[Term] = parameters.foldLeft(List.empty[Term]) {
              case (a, ScalaParameter(_, param, paramName, argName, _)) =>
                val lifter: (Term.Name, RawParameterName) => Term = {
                  param match {
                    case param"$_: Option[fs2.Stream[F,Byte]]" =>
                      liftOptionFileTerm _
                    case param"$_: Option[fs2.Stream[F,Byte]] = $_" =>
                      liftOptionFileTerm _
                    case param"$_: fs2.Stream[F,Byte]"      => liftFileTerm _
                    case param"$_: fs2.Stream[F,Byte] = $_" => liftFileTerm _
                    case param"$_: Option[$_]"              => liftOptionTerm _
                    case param"$_: Option[$_] = $_"         => liftOptionTerm _
                    case _                                  => liftTerm _
                  }
                }
                a :+ lifter(paramName, argName)
            }
            Some(q"List(..$args)")
          } else {
            def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
              q"List((${tName.toLit}, Formatter.show($tParamName)))"

            def liftIterable(tParamName: Term, tName: RawParameterName) =
              q"$tParamName.toList.map(x => (${tName.toLit}, Formatter.show(x)))"

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
                    case param"$_: Iterable[$_]"      => liftIterable _
                    case param"$_: Iterable[$_] = $_" => liftIterable _
                    case _                            => liftTerm _
                  }
                a :+ lifter(paramName, argName)
            }
            Some(q"List(..$args)")
          }

        def generateHeaderParams(parameters: List[ScalaParameter[ScalaLanguage]]): Term = {
          def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
            q"$tParamName.map(v => Header(${tName.toLit}, Formatter.show(v)))"

          def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
            q"Some(Header(${tName.toLit}, Formatter.show($tParamName)))"

          val args: List[Term] = parameters.foldLeft(List.empty[Term]) {
            case (a, ScalaParameter(_, param, paramName, argName, _)) =>
              val lifter: (Term.Name, RawParameterName) => Term = param match {
                case param"$_: Option[$_]"      => liftOptionTerm _
                case param"$_: Option[$_] = $_" => liftOptionTerm _
                case _                          => liftTerm _
              }
              a :+ lifter(paramName, argName)
          }
          q"List[Option[Header]](..$args).flatten"
        }

        def build(
            methodName: String,
            httpMethod: HttpMethod,
            urlWithParams: Term,
            formDataParams: Option[Term],
            headerParams: Term,
            responses: Responses[ScalaLanguage],
            produces: Seq[RouteMeta.ContentType],
            consumes: Seq[RouteMeta.ContentType],
            tracing: Boolean
        )(
            tracingArgsPre: List[ScalaParameter[ScalaLanguage]],
            tracingArgsPost: List[ScalaParameter[ScalaLanguage]],
            pathArgs: List[ScalaParameter[ScalaLanguage]],
            qsArgs: List[ScalaParameter[ScalaLanguage]],
            formArgs: List[ScalaParameter[ScalaLanguage]],
            body: Option[ScalaParameter[ScalaLanguage]],
            headerArgs: List[ScalaParameter[ScalaLanguage]],
            extraImplicits: List[Term.Param]
        ): Target[RenderedClientOperation[ScalaLanguage]] =
          for {
            _ <- Target.pure(())
            implicitParams                 = Option(extraImplicits).filter(_.nonEmpty)
            defaultHeaders                 = param"headers: List[Header] = List.empty"
            safeBody: Option[(Term, Type)] = body.map(sp => (sp.paramName, sp.argType))

            formDataNeedsMultipart = consumes.contains(RouteMeta.MultipartFormData)
            formEntity: Option[Term] = formDataParams.map { formDataParams =>
              if (formDataNeedsMultipart) {
                q"""_multipart"""
              } else {
                q"""UrlForm($formDataParams.flatten: _*)"""
              }
            }

            (tracingExpr, httpClientName) = if (tracing)
              (List(q"""val tracingHttpClient = traceBuilder(s"$${clientName}:$${methodName}")(httpClient)"""), q"tracingHttpClient")
            else
              (List(), q"httpClient")
            multipartExpr = formDataParams
              .filter(_ => formDataNeedsMultipart)
              .map(formDataParams => q"""val _multipart = Multipart($formDataParams.flatten.toVector)""")
            headersExpr = if (formDataNeedsMultipart) {
              List(q"val allHeaders = headers ++ $headerParams ++ _multipart.headers.toList")
            } else {
              List(q"val allHeaders = headers ++ $headerParams")
            }
            req = q"Request[F](method = Method.${Term.Name(httpMethod.toString.toUpperCase)}, uri = ${urlWithParams}, headers = Headers(allHeaders))"
            reqWithBody = formEntity
              .map(e => q"$req.withEntity($e)")
              .orElse(safeBody.map(_._1).map(e => q"$req.withEntity($e)(${Term.Name(s"${methodName}Encoder")})"))
              .getOrElse(req)
            reqExpr = List(
              q"val req = $reqWithBody"
            )

            buildHeaders = (_: List[Header[ScalaLanguage]])
              .map { header =>
                val headerName = Lit.String(header.name.toLowerCase)
                if (header.isRequired) {
                  q"parseRequiredHeader(resp, $headerName)"
                } else {
                  q"parseOptionalHeader(resp, $headerName)"
                }
              }
            responseCompanionTerm = Term.Name(s"${methodName.capitalize}Response")
            cases <- responses.value.traverse[Target, Case]({ resp =>
              val responseTerm = Term.Name(s"${resp.statusCodeName.value}")
              val statusCode   = Term.Select(p"_root_.org.http4s.Status", resp.statusCodeName)
              (resp.value, resp.headers.value) match {
                case (None, Nil) =>
                  Target.pure(p"case ${statusCode}(_) => F.pure($responseCompanionTerm.$responseTerm)")
                case (maybeBody, headers) =>
                  if (maybeBody.size + headers.size > 22) {
                    // we have hit case class limitation
                    // https://github.com/twilio/guardrail/pull/382
                    Target.raiseError(
                      s"Failed to generate client for method $methodName and status code ${resp.statusCode}. It's currently not possible to have more than 22 properties (payload, HTTP headers). See https://github.com/twilio/guardrail/pull/382."
                    )
                  } else {
                    val decodeValue = maybeBody.map { _ =>
                      q"${Term.Name(s"$methodName${resp.statusCodeName}Decoder")}.decode(resp, strict = false).value.flatMap(F.fromEither)"
                    }
                    val decodeHeaders = buildHeaders(headers)
                    val mapArgs       = decodeValue.toList ++ decodeHeaders
                    val mapTerm       = if (mapArgs.size == 1) q"map" else Term.Name(s"map${mapArgs.size}")
                    Target.pure(p"case $statusCode(resp) => F.$mapTerm(..$mapArgs)($responseCompanionTerm.$responseTerm.apply)")
                  }
              }
            })
            unexpectedCase = p"case resp => F.raiseError(UnexpectedStatus(resp.status))"
            // Get the response type
            isGeneric           = Http4sHelper.isDefinitionGeneric(responses)
            baseResponseTypeRef = Type.Name(s"${methodName.capitalize}Response")
            responseTypeRef     = if (isGeneric) t"cats.effect.Resource[F, $baseResponseTypeRef[F]]" else t"F[$baseResponseTypeRef]"
            executeReqExpr = if (isGeneric) List(q"""$httpClientName.run(req).evalMap(${Term.PartialFunction(cases :+ unexpectedCase)})""")
            else List(q"""$httpClientName.fetch(req)(${Term.PartialFunction(cases :+ unexpectedCase)})""")
            methodBody: Term = q"""
              {
                ..${tracingExpr ++ multipartExpr ++ headersExpr ++ reqExpr ++ executeReqExpr}
              }
              """

            formParams = formArgs.map(
              scalaParam =>
                scalaParam.param.copy(
                  decltpe =
                    (
                      if (scalaParam.isFile) {
                        if (scalaParam.required) {
                          Some(t"(String, Stream[F, Byte])")
                        } else {
                          Some(t"Option[(String, Stream[F, Byte])]")
                        }
                      } else {
                        scalaParam.param.decltpe
                      }
                    )
                )
            )

            arglists: List[List[Term.Param]] = List(
              Some(
                (tracingArgsPre.map(_.param) ++ pathArgs.map(_.param) ++ qsArgs
                      .map(_.param) ++ formParams ++ body
                      .map(_.param) ++ headerArgs.map(_.param) ++ tracingArgsPost
                      .map(_.param)) :+ defaultHeaders
              ),
              implicitParams
            ).flatten
          } yield {
            RenderedClientOperation[ScalaLanguage](
              q"""
                def ${Term
                .Name(methodName)}(...${arglists}): $responseTypeRef = $methodBody
              """,
              generateCodecs(methodName, body, responses, produces, consumes)
            )
          }

        Target.log.function("generateClientOperation")(for {
          // Placeholder for when more functions get logging
          _ <- Target.pure(())

          consumes = operation.get.consumes.toList.flatMap(RouteMeta.ContentType.unapply(_))
          produces = operation.get.produces.toList.flatMap(RouteMeta.ContentType.unapply(_))

          headerArgs = parameters.headerParams
          pathArgs   = parameters.pathParams
          qsArgs     = parameters.queryStringParams
          bodyArgs   = parameters.bodyParams
          formArgs   = parameters.formParams

          _ <- Target.log.debug(s"pathArgs: ${pathArgs}")

          // Generate the url with path, query parameters
          urlWithParams <- generateUrlWithParams(pathStr, pathArgs, qsArgs)

          _ <- Target.log.debug(s"Generated: ${urlWithParams}")
          // Generate FormData arguments
          formDataParams = generateFormDataParams(formArgs, consumes.contains(RouteMeta.MultipartFormData))
          // Generate header arguments
          headerParams = generateHeaderParams(headerArgs)

          tracingArgsPre = if (tracing)
            List(ScalaParameter.fromParam(param"traceBuilder: TraceBuilder[F]"))
          else List.empty
          tracingArgsPost = if (tracing)
            List(ScalaParameter.fromParam(param"methodName: String = ${Lit.String(methodName.toDashedCase)}"))
          else List.empty
          extraImplicits = List.empty

          renderedClientOperation <- build(methodName, httpMethod, urlWithParams, formDataParams, headerParams, responses, produces, consumes, tracing)(
            tracingArgsPre,
            tracingArgsPost,
            pathArgs,
            qsArgs,
            formArgs,
            bodyArgs,
            headerArgs,
            extraImplicits
          )
        } yield renderedClientOperation)

      case GetImports(tracing) => Target.pure(List.empty)

      case GetExtraImports(tracing) => Target.pure(List.empty)

      case ClientClsArgs(tracingName, serverUrls, tracing) =>
        val ihc = param"implicit httpClient: Http4sClient[F]"
        val ief = param"implicit F: Async[F]"
        Target.pure(
          List(
            List(formatHost(serverUrls)) ++ (if (tracing)
                                               Some(formatClientName(tracingName))
                                             else None),
            List(ief, ihc)
          )
        )

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        Target.pure(Http4sHelper.generateResponseDefinitions(operationId, responses, protocolElems))

      case GenerateSupportDefinitions(tracing, securitySchemes) =>
        Target.pure(List.empty)

      case BuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing) =>
        def extraConstructors(
            tracingName: Option[String],
            serverUrls: Option[NonEmptyList[URI]],
            tpe: Type.Name,
            ctorCall: Term.New,
            tracing: Boolean
        ): List[Defn] = {
          val tracingParams: List[Term.Param] = if (tracing) {
            List(formatClientName(tracingName))
          } else {
            List.empty
          }

          List(
            q"""
              def httpClient[F[_]](httpClient: Http4sClient[F], ${formatHost(serverUrls)}, ..${tracingParams})(implicit F: Async[F]): ${tpe}[F] = ${ctorCall}
            """
          )
        }

        def paramsToArgs(params: List[List[Term.Param]]): List[List[Term]] =
          params
            .map({
              _.map(_.name.value)
                .map(v => Term.Assign(Term.Name(v), Term.Name(v)))
                .to[List]
            })
            .to[List]

        val ctorCall: Term.New = {
          q"""
            new ${Type
            .Apply(Type.Name(clientName), List(Type.Name("F")))}(...${paramsToArgs(ctorArgs)})
          """
        }

        val decls: List[Defn] =
          q"""def apply[F[_]](...${ctorArgs}): ${Type.Apply(Type.Name(clientName), List(Type.Name("F")))} = ${ctorCall}""" +:
              extraConstructors(tracingName, serverUrls, Type.Name(clientName), ctorCall, tracing)
        Target.pure(
          StaticDefns[ScalaLanguage](
            className = clientName,
            extraImports = List.empty,
            definitions = decls
          )
        )

      case BuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing) =>
        val client =
          q"""
            class ${Type.Name(clientName)}[F[_]](...${ctorArgs}) {
              val basePath: String = ${Lit.String(basePath.getOrElse(""))}

              private def parseOptionalHeader(response: Response[F], header: String): F[Option[String]] =
                F.pure(response.headers.get(header.ci).map(_.value))

              private def parseRequiredHeader(response: Response[F], header: String): F[String] =
                response.headers
                  .get(header.ci)
                  .map(_.value)
                  .fold[F[String]](
                    F.raiseError(
                      ParseFailure(
                        "Missing required header.",
                        s"HTTP header '$$header' is not present."
                      )
                    )
                  )(F.pure)

              ..${supportDefinitions};
              ..$clientCalls
            }
          """
        Target.pure(NonEmptyList(Right(client), Nil))
    }

    def generateCodecs(
        methodName: String,
        bodyArgs: Option[ScalaParameter[ScalaLanguage]],
        responses: Responses[ScalaLanguage],
        produces: Seq[RouteMeta.ContentType],
        consumes: Seq[RouteMeta.ContentType]
    ): List[Defn.Val] =
      generateEncoders(methodName, bodyArgs, consumes) ++ generateDecoders(methodName, responses, produces)

    def generateEncoders(methodName: String, bodyArgs: Option[ScalaParameter[ScalaLanguage]], consumes: Seq[RouteMeta.ContentType]): List[Defn.Val] =
      bodyArgs.toList.flatMap {
        case ScalaParameter(_, _, _, _, argType) =>
          List(q"private[this] val ${Pat.Var(Term.Name(s"${methodName}Encoder"))} = ${Http4sHelper.generateEncoder(argType, consumes)}")
      }

    def generateDecoders(methodName: String, responses: Responses[ScalaLanguage], produces: Seq[RouteMeta.ContentType]): List[Defn.Val] =
      for {
        resp <- responses.value
        tpe  <- resp.value.map(_._1)
      } yield q"private[this] val ${Pat.Var(Term.Name(s"$methodName${resp.statusCodeName}Decoder"))} = ${Http4sHelper.generateDecoder(tpe, produces)}"
  }

}
