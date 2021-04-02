package com.twilio.guardrail.generators.Scala

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import com.twilio.guardrail.{ RenderedClientOperation, StaticDefns, StrictProtocolElems, SupportDefinition, Target }
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.{ ContentType, Header, MultipartFormData, Responses }
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }
import com.twilio.guardrail.generators.{ LanguageParameter, LanguageParameters, RawParameterName }
import scala.meta._
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import java.net.URI

object Http4sClientGenerator {

  def ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ClientTerms[ScalaLanguage, Target] = new ClientTermInterp
  class ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ClientTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

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

    def generateClientOperation(
        className: List[String],
        responseClsName: String,
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
        parameters: LanguageParameters[ScalaLanguage]
    )(
        route: RouteMeta,
        methodName: String,
        responses: Responses[ScalaLanguage]
    ): Target[RenderedClientOperation[ScalaLanguage]] = {
      val RouteMeta(pathStr, httpMethod, operation, securityRequirements) = route
      val containerTransformations = Map[String, Term => Term](
        "Iterable"   -> identity _,
        "List"       -> (term => q"$term.toList"),
        "Vector"     -> (term => q"$term.toVector"),
        "Seq"        -> (term => q"$term.toSeq"),
        "IndexedSeq" -> (term => q"$term.toIndexedSeq")
      )

      def generateUrlWithParams(
          path: Tracker[String],
          pathArgs: List[LanguageParameter[ScalaLanguage]],
          qsArgs: List[LanguageParameter[ScalaLanguage]]
      ): Target[Term] =
        Target.log.function("generateUrlWithParams") {
          for {
            _    <- Target.log.debug(s"Using ${path.unwrapTracker} and ${pathArgs.map(_.argName)}")
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
                  case (a, LanguageParameter(_, _, paramName, argName, _)) =>
                    q""" $a + Formatter.addArg(${Lit
                      .String(argName.value)}, ${paramName})"""
                }
              })
          } yield q"Uri.unsafeFromString(${result})"
        }

      def generateFormDataParams(parameters: List[LanguageParameter[ScalaLanguage]], consumes: List[ContentType]): Option[Term] =
        if (parameters.isEmpty) {
          None
        } else if (consumes.contains(MultipartFormData)) {
          def liftOptionFileTerm(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.map(v => Part.fileData[F](${tName.toLit}, v._1, v._2))"

          def liftFileTerm(tParamName: Term, tName: RawParameterName) =
            q"Some(Part.fileData[F](${tName.toLit}, ${tParamName}._1, ${tParamName}._2))"

          def liftOptionTerm(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.map(v => Part.formData[F](${tName.toLit}, Formatter.show(v)))"

          def liftTerm(tParamName: Term, tName: RawParameterName) =
            q"Some(Part.formData[F](${tName.toLit}, Formatter.show($tParamName)))"

          val lifter: Term.Param => (Term, RawParameterName) => Term = {
            {
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

          val args: List[Term] = parameters.map {
            case LanguageParameter(_, param, paramName, argName, _) =>
              lifter(param)(paramName, argName)
          }
          Some(q"List(..$args)")
        } else {
          def liftTerm(tParamName: Term, tName: RawParameterName) =
            q"List((${tName.toLit}, Formatter.show($tParamName)))"

          def liftIterable(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.toList.map(x => (${tName.toLit}, Formatter.show(x)))"

          def liftOptionTerm(tpe: Type)(tParamName: Term, tName: RawParameterName) = {
            val lifter = tpe match {
              case t"$container[$_]" if containerTransformations.contains(container.syntax) => liftIterable _
              case _                                                                        => liftTerm _
            }
            q"${tParamName}.toList.flatMap(${Term.Block(List(q" x => ${lifter(Term.Name("x"), tName)}"))})"
          }

          val lifter: Term.Param => (Term, RawParameterName) => Term = {
            case param"$_: Option[$tpe]"                                                                 => liftOptionTerm(tpe) _
            case param"$_: Option[$tpe] = $_"                                                            => liftOptionTerm(tpe) _
            case param"$_: $container[$tpe]" if containerTransformations.contains(container.syntax)      => liftIterable _
            case param"$_: $container[$tpe] = $_" if containerTransformations.contains(container.syntax) => liftIterable _
            case _                                                                                       => liftTerm _
          }

          val args: List[Term] = parameters.map {
            case LanguageParameter(_, param, paramName, argName, _) =>
              lifter(param)(paramName, argName)
          }
          Some(q"List(..$args)")
        }

      def generateHeaderParams(parameters: List[LanguageParameter[ScalaLanguage]]): Term = {
        def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
          q"$tParamName.map(v => Header(${tName.toLit}, Formatter.show(v)))"

        def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
          q"Some(Header(${tName.toLit}, Formatter.show($tParamName)))"

        val lifter: Term.Param => (Term.Name, RawParameterName) => Term = {
          case param"$_: Option[$_]"      => liftOptionTerm _
          case param"$_: Option[$_] = $_" => liftOptionTerm _
          case _                          => liftTerm _
        }

        val args: List[Term] = parameters.map {
          case LanguageParameter(_, param, paramName, argName, _) =>
            lifter(param)(paramName, argName)
        }
        q"List[Option[Header]](..$args).flatten"
      }

      def build(
          methodName: String,
          responseClsName: String,
          httpMethod: HttpMethod,
          urlWithParams: Term,
          formDataParams: Option[Term],
          headerParams: Term,
          responses: Responses[ScalaLanguage],
          produces: Seq[ContentType],
          consumes: Seq[ContentType],
          tracing: Boolean
      )(
          tracingArgsPre: List[LanguageParameter[ScalaLanguage]],
          tracingArgsPost: List[LanguageParameter[ScalaLanguage]],
          pathArgs: List[LanguageParameter[ScalaLanguage]],
          qsArgs: List[LanguageParameter[ScalaLanguage]],
          formArgs: List[LanguageParameter[ScalaLanguage]],
          body: Option[LanguageParameter[ScalaLanguage]],
          headerArgs: List[LanguageParameter[ScalaLanguage]],
          extraImplicits: List[Term.Param]
      ): Target[RenderedClientOperation[ScalaLanguage]] =
        for {
          _ <- Target.pure(())
          implicitParams                 = Option(extraImplicits).filter(_.nonEmpty)
          defaultHeaders                 = param"headers: List[Header] = List.empty"
          safeBody: Option[(Term, Type)] = body.map(sp => (sp.paramName, sp.argType))

          formDataNeedsMultipart = consumes.contains(MultipartFormData)
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
          responseCompanionTerm = Term.Name(responseClsName)
          isGeneric             = Http4sHelper.isDefinitionGeneric(responses)
          baseResponseTypeRef   = Type.Name(responseClsName)
          cases <- responses.value.traverse[Target, Case]({ resp =>
            val responseTerm = Term.Name(s"${resp.statusCodeName.value}")
            val statusCode   = Term.Select(p"_root_.org.http4s.Status", resp.statusCodeName)
            (resp.value, resp.headers.value) match {
              case (None, Nil) =>
                if (isGeneric)
                  Target.pure(p"case ${statusCode}(_) => F.pure($responseCompanionTerm.$responseTerm): F[$baseResponseTypeRef[F]]")
                else
                  Target.pure(p"case ${statusCode}(_) => F.pure($responseCompanionTerm.$responseTerm): F[$baseResponseTypeRef]")
              case (maybeBody, headers) =>
                if (maybeBody.size + headers.size > 22) {
                  // we have hit case class limitation
                  // https://github.com/guardrail-dev/guardrail/pull/382
                  Target.raiseUserError(
                    s"Failed to generate client for method $methodName and status code ${resp.statusCode}. It's currently not possible to have more than 22 properties (payload, HTTP headers). See https://github.com/guardrail-dev/guardrail/pull/382."
                  )
                } else {
                  val decodeValue = maybeBody.map { _ =>
                    q"${Term.Name(s"$methodName${resp.statusCodeName}Decoder")}.decode(resp, strict = false).value.flatMap(F.fromEither)"
                  }
                  val decodeHeaders = buildHeaders(headers)
                  val mapArgs       = decodeValue.toList ++ decodeHeaders
                  val mapTerm       = if (mapArgs.size == 1) q"map" else Term.Name(s"map${mapArgs.size}")

                  if (isGeneric)
                    Target.pure(p"case $statusCode(resp) => F.$mapTerm(..$mapArgs)($responseCompanionTerm.$responseTerm.apply): F[$baseResponseTypeRef[F]]")
                  else
                    Target.pure(p"case $statusCode(resp) => F.$mapTerm(..$mapArgs)($responseCompanionTerm.$responseTerm.apply): F[$baseResponseTypeRef]")

                }
            }
          })
          // Get the response type
          unexpectedCase = if (isGeneric) p"case resp => F.raiseError[$baseResponseTypeRef[F]](UnexpectedStatus(resp.status))"
          else p"case resp => F.raiseError[$baseResponseTypeRef](UnexpectedStatus(resp.status))"
          responseTypeRef = if (isGeneric) t"cats.effect.Resource[F, $baseResponseTypeRef[F]]" else t"F[$baseResponseTypeRef]"
          executeReqExpr = if (isGeneric) List(q"""$httpClientName.run(req).evalMap(${Term.PartialFunction(cases :+ unexpectedCase)})""")
          else List(q"""$httpClientName.run(req).use(${Term.PartialFunction(cases :+ unexpectedCase)})""")
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

        consumes = operation.unwrapTracker.consumes.toList.flatMap(ContentType.unapply(_))
        produces = operation.unwrapTracker.produces.toList.flatMap(ContentType.unapply(_))

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
        formDataParams = generateFormDataParams(formArgs, consumes)
        // Generate header arguments
        headerParams = generateHeaderParams(headerArgs)

        tracingArgsPre = if (tracing)
          List(LanguageParameter.fromParam(param"traceBuilder: TraceBuilder[F]"))
        else List.empty
        tracingArgsPost = if (tracing)
          List(LanguageParameter.fromParam(param"methodName: String = ${Lit.String(methodName.toDashedCase)}"))
        else List.empty
        extraImplicits = List.empty

        renderedClientOperation <- build(
          methodName,
          responseClsName,
          httpMethod,
          urlWithParams,
          formDataParams,
          headerParams,
          responses,
          produces,
          consumes,
          tracing
        )(
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
    }
    def getImports(tracing: Boolean): Target[List[scala.meta.Import]]      = Target.pure(List(q"import org.http4s.circe._"))
    def getExtraImports(tracing: Boolean): Target[List[scala.meta.Import]] = Target.pure(List.empty)
    def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): Target[List[List[scala.meta.Term.Param]]] = {
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
    }
    def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[ScalaLanguage],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]]
    ): Target[List[scala.meta.Defn]] =
      Target.pure(Http4sHelper.generateResponseDefinitions(responseClsName, responses, protocolElems))

    def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ): Target[List[SupportDefinition[ScalaLanguage]]] = Target.pure(List.empty)

    def buildStaticDefns(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        ctorArgs: List[List[scala.meta.Term.Param]],
        tracing: Boolean
    ): Target[StaticDefns[ScalaLanguage]] = {
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
              .toList
          })
          .toList

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
    }
    def buildClient(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        basePath: Option[String],
        ctorArgs: List[List[scala.meta.Term.Param]],
        clientCalls: List[scala.meta.Defn],
        supportDefinitions: List[scala.meta.Defn],
        tracing: Boolean
    ): Target[NonEmptyList[Either[scala.meta.Defn.Trait, scala.meta.Defn.Class]]] = {
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
        bodyArgs: Option[LanguageParameter[ScalaLanguage]],
        responses: Responses[ScalaLanguage],
        produces: Seq[ContentType],
        consumes: Seq[ContentType]
    ): List[Defn.Val] =
      generateEncoders(methodName, bodyArgs, consumes) ++ generateDecoders(methodName, responses, produces)

    def generateEncoders(methodName: String, bodyArgs: Option[LanguageParameter[ScalaLanguage]], consumes: Seq[ContentType]): List[Defn.Val] =
      bodyArgs.toList.flatMap {
        case LanguageParameter(_, _, _, _, argType) =>
          List(q"private[this] val ${Pat.Var(Term.Name(s"${methodName}Encoder"))} = ${Http4sHelper.generateEncoder(argType, consumes)}")
      }

    def generateDecoders(methodName: String, responses: Responses[ScalaLanguage], produces: Seq[ContentType]): List[Defn.Val] =
      for {
        resp <- responses.value
        tpe  <- resp.value.map(_._2)
      } yield {
        val contentTypes = resp.value.map(_._1).map(List(_)).getOrElse(produces) //for OpenAPI 3.x we should take ContentType from the response
        q"private[this] val ${Pat.Var(Term.Name(s"$methodName${resp.statusCodeName}Decoder"))} = ${Http4sHelper.generateDecoder(tpe, contentTypes)}"
      }
  }

}
