package com.twilio.guardrail
package generators

import cats.Monad
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.protocol.terms.{ ApplicationJson, ContentType, Header, MultipartFormData, Responses, TextPlain }
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.{ RouteMeta, SecurityScheme }
import com.twilio.guardrail.languages.ScalaLanguage
import scala.meta._
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import java.net.URI

object AkkaHttpClientGenerator {

  object ClientTermInterp extends ClientTerms[ScalaLanguage, Target] with FunctionK[ClientTerm[ScalaLanguage, ?], Target] {
    implicit def ClientTermsMonad: Monad[Target] = Target.targetInstances

    type L = ScalaLanguage

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

    def generateClientOperation(className: List[String], tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]], parameters: ScalaParameters[L])(
        route: RouteMeta,
        methodName: String,
        responses: Responses[L]
    ): Target[RenderedClientOperation[L]] = {
      val RouteMeta(pathStr, httpMethod, operation, securityRequirements) = route
      def generateUrlWithParams(
          path: Tracker[String],
          pathArgs: List[ScalaParameter[ScalaLanguage]],
          qsArgs: List[ScalaParameter[ScalaLanguage]]
      ): Target[Term] =
        Target.log.function("generateUrlWithParams") {
          for {
            _    <- Target.log.debug(s"Using ${path.unwrapTracker} and ${pathArgs.map(_.argName)}")
            base <- generateUrlPathParams(path, pathArgs)

            _ <- Target.log.debug(s"QS: $qsArgs")

            suffix = if (path.unwrapTracker.contains("?")) {
              Lit.String("&")
            } else {
              Lit.String("?")
            }

            _ <- Target.log.debug(s"QS: ${qsArgs}")

            result = NonEmptyList
              .fromList(qsArgs)
              .fold(base)({
                _.foldLeft[Term](q"$base + $suffix") {
                  case (a, ScalaParameter(_, _, paramName, argName, _)) =>
                    q""" $a + Formatter.addArg(${Lit
                      .String(argName.value)}, $paramName)"""
                }
              })
          } yield result
        }

      def generateFormDataParams(parameters: List[ScalaParameter[ScalaLanguage]], consumes: List[ContentType]): Option[Term] =
        if (parameters.isEmpty) {
          None
        } else if (consumes.contains(MultipartFormData)) {
          def liftOptionFileTerm(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.map(v => Multipart.FormData.BodyPart(${tName.toLit}, v))"

          def liftFileTerm(tParamName: Term, tName: RawParameterName) =
            q"Some(Multipart.FormData.BodyPart(${tName.toLit}, $tParamName))"

          def liftOptionTerm(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.map(v => Multipart.FormData.BodyPart(${tName.toLit}, Formatter.show(v)))"

          def liftTerm(tParamName: Term, tName: RawParameterName) =
            q"Some(Multipart.FormData.BodyPart(${tName.toLit}, Formatter.show($tParamName)))"

          val lifter: Term.Param => (Term, RawParameterName) => Term = {
            case param"$_: Option[BodyPartEntity]" =>
              liftOptionFileTerm _
            case param"$_: Option[BodyPartEntity] = $_" =>
              liftOptionFileTerm _
            case param"$_: BodyPartEntity"      => liftFileTerm _
            case param"$_: BodyPartEntity = $_" => liftFileTerm _
            case param"$_: Option[$_]"          => liftOptionTerm _
            case param"$_: Option[$_] = $_"     => liftOptionTerm _
            case _                              => liftTerm _
          }

          val args: List[Term] = parameters.map {
            case ScalaParameter(_, param, paramName, argName, _) =>
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
              case t"Iterable[$_]" => liftIterable _
              case _               => liftTerm _
            }
            q"${tParamName}.toList.flatMap(${Term.Block(List(q" x => ${lifter(Term.Name("x"), tName)}"))})"
          }

          val lifter: Term.Param => (Term, RawParameterName) => Term = {
            case param"$_: Option[$tpe]"        => liftOptionTerm(tpe) _
            case param"$_: Option[$tpe] = $_"   => liftOptionTerm(tpe) _
            case param"$_: Iterable[$tpe]"      => liftIterable _
            case param"$_: Iterable[$tpe] = $_" => liftIterable _
            case x                              => liftTerm _
          }

          val args: List[Term] = parameters.map {
            case ScalaParameter(_, param, paramName, argName, _) =>
              lifter(param)(paramName, argName)
          }
          Some(q"List(..$args).flatten")
        }

      def generateHeaderParams(parameters: List[ScalaParameter[ScalaLanguage]]): Term = {
        def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
          q"$tParamName.map(v => RawHeader(${tName.toLit}, Formatter.show(v)))"

        def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
          q"Some(RawHeader(${tName.toLit}, Formatter.show($tParamName)))"

        val lifter: Term.Param => (Term.Name, RawParameterName) => Term = {
          case param"$_: Option[$_]"      => liftOptionTerm _
          case param"$_: Option[$_] = $_" => liftOptionTerm _
          case _                          => liftTerm _
        }

        val args: List[Term] = parameters.map {
          case ScalaParameter(_, param, paramName, argName, _) =>
            lifter(param)(paramName, argName)
        }
        q"scala.collection.immutable.Seq[Option[HttpHeader]](..$args).flatten"
      }

      def build(
          methodName: String,
          httpMethod: HttpMethod,
          urlWithParams: Term,
          formDataParams: Option[Term],
          headerParams: Term,
          responses: Responses[ScalaLanguage],
          produces: NonEmptyList[ContentType],
          consumes: Seq[ContentType],
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
      ): Target[RenderedClientOperation[ScalaLanguage]] = {
        val implicitParams = Option(extraImplicits).filter(_.nonEmpty)
        val defaultHeaders = param"headers: List[HttpHeader] = Nil"
        val fallbackHttpBody: Option[(Term, Type)] =
          if (Set(HttpMethod.PUT, HttpMethod.POST) contains httpMethod)
            Some((q"HttpEntity.Empty", t"HttpEntity.Strict"))
          else None
        val textPlainBody: Option[Term] =
          if (consumes.contains(TextPlain))
            body.map { sp =>
              val inner = if (sp.required) sp.paramName else q"${sp.paramName}.getOrElse(${Lit.String("")})"
              q"TextPlain(${inner})"
            }
          else None
        val safeBody: Option[(Term, Type)] =
          body.map(sp => (sp.paramName, sp.argType)).orElse(fallbackHttpBody)

        val formEntity: Option[Term] = formDataParams.map { formDataParams =>
          if (consumes.contains(MultipartFormData)) {
            q"""Multipart.FormData(Source.fromIterator { () => $formDataParams.flatten.iterator })"""
          } else {
            q"""FormData($formDataParams: _*)"""
          }
        }

        val (tracingExpr, httpClientName) =
          if (tracing)
            (List(q"""val tracingHttpClient = traceBuilder(s"$${clientName}:$${methodName}")(httpClient)"""), q"tracingHttpClient")
          else
            (List.empty, q"httpClient")

        val headersExpr = List(q"val allHeaders = headers ++ $headerParams")

        val entity: Term = formEntity
          .orElse(textPlainBody)
          .orElse(safeBody.map(_._1))
          .getOrElse(q"HttpEntity.Empty")

        def buildOptionalHeaders(headers: List[Header[ScalaLanguage]]) =
          headers.map { header =>
            val lit = Lit.String(header.name.toLowerCase)
            q"val ${Pat.Var(header.term)} = resp.headers.find(_.is($lit)).map(_.value())"
          }
        def buildRequiredHeaders(headers: List[Header[ScalaLanguage]]) =
          headers.map { header =>
            val lit  = Lit.String(header.name.toLowerCase)
            val expr = q"resp.headers.find(_.is($lit)).map(_.value())"
            val errLiteral =
              Lit.String(s"Expected required HTTP header '${header.name}'")
            enumerator"${Pat.Var(header.term)} <- $expr.toRight(Left(new Exception($errLiteral)))"
          }
        def buildHeaders(headers: List[Header[ScalaLanguage]], term: Term) = {
          val (required, optional) = headers.partition(_.isRequired)

          val optionalVals  = buildOptionalHeaders(optional)
          val forGenerators = buildRequiredHeaders(required)
          val body = if (forGenerators.isEmpty) {
            q"Right($term)"
          } else {
            q"""for {
                  ..$forGenerators
                  } yield $term"""
          }
          (optionalVals, body)
        }

        val responseCompanionTerm =
          Term.Name(s"${methodName.capitalize}Response")
        val cases = responses.value.map { resp =>
            val responseTerm = Term.Name(s"${resp.statusCodeName.value}")
            (resp.value, resp.headers.value) match {
              case (None, Nil) =>
                p"case StatusCodes.${resp.statusCodeName} => resp.discardEntityBytes().future.map(_ => Right($responseCompanionTerm.$responseTerm))"
              case (Some((tpe, _)), Nil) =>
                p"case StatusCodes.${resp.statusCodeName} => Unmarshal(resp.entity).to[${tpe}](${Term
                  .Name(s"$methodName${resp.statusCodeName}Decoder")}, implicitly, implicitly).map(x => Right($responseCompanionTerm.$responseTerm(x)))"
              case (None, headers) =>
                val (optionalVals, body) = buildHeaders(
                  headers,
                  q"$responseCompanionTerm.$responseTerm(..${headers.map(_.term)})"
                )
                p"""case StatusCodes.${resp.statusCodeName} =>
                   ..$optionalVals
                   resp.discardEntityBytes().future.map(_ => $body)
                """
              case (Some((tpe, _)), headers) =>
                val (optionalVals, body) = buildHeaders(
                  headers,
                  q"$responseCompanionTerm.$responseTerm(..${Term
                    .Name("x") :: headers.map(_.term)})"
                )
                p"""case StatusCodes.${resp.statusCodeName} =>
                  ..$optionalVals
                  Unmarshal(resp.entity).to[${tpe}](${Term.Name(
                  s"$methodName${resp.statusCodeName}Decoder"
                )}, implicitly, implicitly).map(x => $body)"""
            }
          } :+ p"case _ => FastFuture.successful(Left(Right(resp)))"
        val responseTypeRef = Type.Name(s"${methodName.capitalize}Response")

        val methodBody = q"""
          {
            ..${tracingExpr};
            ..${headersExpr};
            makeRequest(
              HttpMethods.${Term.Name(httpMethod.toString.toUpperCase)},
              ${urlWithParams},
              allHeaders,
              ${entity},
              HttpProtocols.`HTTP/1.1`
            ).flatMap(req =>
              EitherT(${httpClientName}(req).flatMap(resp =>
                ${Term.Match(q"resp.status", cases)}
              ).recover({
                case e: Throwable =>
                  Left(Left(e))
              }))
            )
          }
          """

        val arglists: List[List[Term.Param]] = List(
          Some(
            (tracingArgsPre.map(_.param) ++ pathArgs.map(_.param) ++ qsArgs
                  .map(_.param) ++ formArgs.map(_.param) ++ body
                  .map(_.param) ++ headerArgs.map(_.param) ++ tracingArgsPost
                  .map(_.param)) :+ defaultHeaders
          ),
          implicitParams
        ).flatten

        for {
          codecs <- generateCodecs(methodName, responses, produces)
        } yield RenderedClientOperation[ScalaLanguage](
          q"""
            def ${Term.Name(methodName)}(...${arglists}): EitherT[Future, Either[Throwable, HttpResponse], $responseTypeRef] = $methodBody
          """,
          codecs
        )
      }

      Target.log.function("generateClientOperation")(for {
        // Placeholder for when more functions get logging
        _ <- Target.pure(())

        produces = NonEmptyList
          .fromList(operation.get.produces.toList.flatMap(ContentType.unapply(_)))
          .getOrElse(NonEmptyList.one(ApplicationJson))
        consumes = operation.get.consumes.toList.flatMap(ContentType.unapply(_))

        headerArgs = parameters.headerParams
        pathArgs   = parameters.pathParams
        qsArgs     = parameters.queryStringParams
        bodyArgs   = parameters.bodyParams
        formArgs   = parameters.formParams

        _ <- Target.log.debug(s"pathArgs: $pathArgs")

        // Generate the url with path, query parameters
        urlWithParams <- generateUrlWithParams(pathStr, pathArgs, qsArgs)

        _ <- Target.log.debug(s"Generated: $urlWithParams")
        // Generate FormData arguments
        formDataParams = generateFormDataParams(formArgs, consumes)
        // Generate header arguments
        headerParams = generateHeaderParams(headerArgs)

        tracingArgsPre = if (tracing)
          List(ScalaParameter.fromParam(param"traceBuilder: TraceBuilder"))
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
    }
    def getImports(tracing: Boolean): Target[List[L#Import]]      = Target.pure(List.empty)
    def getExtraImports(tracing: Boolean): Target[List[L#Import]] = Target.pure(List.empty)
    def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): Target[List[List[L#MethodParameter]]] = {
      val ihc =
        param"implicit httpClient: HttpRequest => Future[HttpResponse]"
      val iec  = param"implicit ec: ExecutionContext"
      val imat = param"implicit mat: Materializer"
      Target.pure(
        List(
          List(formatHost(serverUrls)) ++ (if (tracing)
                                             Some(formatClientName(tracingName))
                                           else None),
          List(ihc, iec, imat)
        )
      )
    }
    def generateResponseDefinitions(operationId: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]): Target[List[L#Definition]] =
      Target.pure(Http4sHelper.generateResponseDefinitions(operationId, responses, protocolElems))
    def generateSupportDefinitions(tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]]): Target[List[SupportDefinition[L]]] =
      Target.pure(List.empty)
    def buildStaticDefns(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        ctorArgs: List[List[L#MethodParameter]],
        tracing: Boolean
    ): Target[StaticDefns[L]] = {
      def extraConstructors(
          tracingName: Option[String],
          serverUrls: Option[NonEmptyList[URI]],
          tpe: Type.Name,
          ctorCall: Term.New,
          tracing: Boolean
      ): List[Defn] = {
        val iec  = param"implicit ec: ExecutionContext"
        val imat = param"implicit mat: Materializer"
        val tracingParams: List[Term.Param] = if (tracing) {
          List(formatClientName(tracingName))
        } else {
          List.empty
        }

        List(
          q"""
              def httpClient(httpClient: HttpRequest => Future[HttpResponse], ${formatHost(serverUrls)}, ..$tracingParams)($iec, $imat): $tpe = $ctorCall
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
            new ${Type.Name(clientName)}(...${paramsToArgs(ctorArgs)})
          """
      }

      val decls: List[Defn] =
        q"""def apply(...$ctorArgs): ${Type.Name(clientName)} = $ctorCall""" +:
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
        ctorArgs: List[List[L#MethodParameter]],
        clientCalls: List[L#Definition],
        supportDefinitions: List[L#Definition],
        tracing: Boolean
    ): Target[NonEmptyList[Either[L#Trait, L#ClassDefinition]]] = {
      val client =
        q"""
            class ${Type.Name(clientName)}(...$ctorArgs) {
              val basePath: String = ${Lit.String(basePath.getOrElse(""))}

              private[this] def makeRequest[T: ToEntityMarshaller](
                method: HttpMethod,
                uri: Uri,
                headers: scala.collection.immutable.Seq[HttpHeader],
                entity: T,
                protocol: HttpProtocol
              ): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
                EitherT(
                  Marshal(entity)
                    .to[RequestEntity]
                    .map[Either[Either[Throwable, HttpResponse], HttpRequest]]({ entity =>
                      Right(HttpRequest(
                        method=method,
                        uri=uri,
                        headers=headers,
                        entity=entity,
                        protocol=protocol
                      ))
                    })
                    .recover({ case t =>
                      Left(Left(t))
                    })
                )
              }

              ..$supportDefinitions;
              ..$clientCalls;
            }
          """
      Target.pure(NonEmptyList(Right(client), Nil))
    }

    def apply[T](term: ClientTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(className, route, methodName, tracing, parameters, responses, securitySchemes) =>
        generateClientOperation(className, tracing, securitySchemes, parameters)(route, methodName, responses)

      case GetImports(tracing) => getImports(tracing)

      case GetExtraImports(tracing) => getExtraImports(tracing)

      case ClientClsArgs(tracingName, serverUrls, tracing) => clientClsArgs(tracingName, serverUrls, tracing)

      case GenerateResponseDefinitions(operationId, responses, protocolElems) => generateResponseDefinitions(operationId, responses, protocolElems)

      case GenerateSupportDefinitions(tracing, securitySchemes) => generateSupportDefinitions(tracing, securitySchemes)

      case BuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing) => buildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing)

      case BuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing) =>
        buildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing)
    }

    def generateCodecs(methodName: String, responses: Responses[ScalaLanguage], produces: NonEmptyList[ContentType]): Target[List[Defn.Val]] =
      generateDecoders(methodName, responses, produces)

    def generateDecoders(methodName: String, responses: Responses[ScalaLanguage], produces: NonEmptyList[ContentType]): Target[List[Defn.Val]] =
      (for {
        resp <- responses.value
        tpe  <- resp.value.map(_._1).toList
      } yield {
        for {
          (decoder, baseType) <- AkkaHttpHelper.generateDecoder(tpe, produces)
        } yield q"val ${Pat.Var(Term.Name(s"$methodName${resp.statusCodeName}Decoder"))} = ${decoder}"
      }).sequence
  }

}
