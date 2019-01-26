package com.twilio.guardrail
package generators

import _root_.io.swagger.v3.oas.models._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.extract.ScalaPackage
import com.twilio.guardrail.generators.syntax.Scala._
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.client._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.RouteMeta
import java.util.Locale
import scala.collection.JavaConverters._
import scala.meta._
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import java.net.URI

object Http4sClientGenerator {

  object ClientTermInterp extends FunctionK[ClientTerm[ScalaLanguage, ?], Target] {
    def splitOperationParts(operationId: String): (List[String], String) = {
      val parts = operationId.split('.')
      (parts.drop(1).toList, parts.last)
    }

    private[this] def toDashedCase(s: String): String = {
      val lowercased =
        "^([A-Z])".r.replaceAllIn(s, m => m.group(1).toLowerCase(Locale.US))
      "([A-Z])".r
        .replaceAllIn(lowercased, m => '-' +: m.group(1).toLowerCase(Locale.US))
    }

    private[this] def formatClientName(clientName: Option[String]): Term.Param =
      clientName.fold(
        param"clientName: String"
      )(name => param"clientName: String = ${Lit.String(toDashedCase(name))}")

    private[this] def formatHost(serverUrls: Option[NonEmptyList[URI]]): Term.Param =
      serverUrls
        .fold(param"host: String")(v => param"host: String = ${Lit.String(v.head.toString())}")

    def apply[T](term: ClientTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(className, route @ RouteMeta(pathStr, httpMethod, operation), methodName, tracing, parameters, responses) =>
        def generateUrlWithParams(path: String, pathArgs: List[ScalaParameter[ScalaLanguage]], qsArgs: List[ScalaParameter[ScalaLanguage]]): Target[Term] =
          for {
            _    <- Target.log.debug("generateClientOperation", "generateUrlWithParams")(s"Using ${path} and ${pathArgs.map(_.argName)}")
            base <- SwaggerUtil.paths.generateUrlPathParams(path, pathArgs)

            _ <- Target.log.debug("generateClientOperation", "generateUrlWithParams")(s"QS: ${qsArgs}")

            suffix = if (path.contains("?")) {
              Lit.String("&")
            } else {
              Lit.String("?")
            }

            _ <- Target.log.debug("generateClientOperation", "generateUrlWithParams")(s"QS: ${qsArgs}")

            result = NonEmptyList
              .fromList(qsArgs.toList)
              .fold(base)({
                _.foldLeft[Term](q"${base} + ${suffix}") {
                  case (a, ScalaParameter(_, _, paramName, argName, _)) =>
                    q""" $a + Formatter.addArg(${Lit
                      .String(argName.value)}, ${paramName})"""
                }
              })
          } yield q"Uri.unsafeFromString(${result})"

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
                    case param"$_: Option[java.io.File]" =>
                      liftOptionFileTerm _
                    case param"$_: Option[java.io.File] = $_" =>
                      liftOptionFileTerm _
                    case param"$_: java.io.File"      => liftFileTerm _
                    case param"$_: java.io.File = $_" => liftFileTerm _
                    case param"$_: Option[$_]"        => liftOptionTerm _
                    case param"$_: Option[$_] = $_"   => liftOptionTerm _
                    case _                            => liftTerm _
                  }
                }
                a :+ lifter(paramName, argName)
            }
            Some(q"List(..$args)")
          } else {
            def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
              q"(${tName.toLit}, $tParamName.map(Formatter.show(_)))"

            def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
              q"(${tName.toLit}, Some(Formatter.show($tParamName)))"

            val args: List[Term] = parameters.foldLeft(List.empty[Term]) {
              case (a, ScalaParameter(_, param, paramName, argName, _)) =>
                val lifter: (Term.Name, RawParameterName) => Term =
                  param match {
                    case param"$_: Option[$_]"      => liftOptionTerm _
                    case param"$_: Option[$_] = $_" => liftOptionTerm _
                    case _                          => liftTerm _
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

        def build(methodName: String,
                  httpMethod: HttpMethod,
                  urlWithParams: Term,
                  formDataParams: Option[Term],
                  headerParams: Term,
                  responses: Responses[ScalaLanguage],
                  produces: Seq[String],
                  consumes: Seq[String],
                  tracing: Boolean)(tracingArgsPre: List[ScalaParameter[ScalaLanguage]],
                                    tracingArgsPost: List[ScalaParameter[ScalaLanguage]],
                                    pathArgs: List[ScalaParameter[ScalaLanguage]],
                                    qsArgs: List[ScalaParameter[ScalaLanguage]],
                                    formArgs: List[ScalaParameter[ScalaLanguage]],
                                    body: Option[ScalaParameter[ScalaLanguage]],
                                    headerArgs: List[ScalaParameter[ScalaLanguage]],
                                    extraImplicits: List[Term.Param]): RenderedClientOperation[ScalaLanguage] = {
          val implicitParams = Option(extraImplicits).filter(_.nonEmpty)
          val defaultHeaders = param"headers: List[Header] = List.empty"
          val safeBody: Option[(Term, Type)] =
            body.map(sp => (sp.paramName, sp.argType))

          val formDataNeedsMultipart = consumes.contains("multipart/form-data")
          val formEntity: Option[Term] = formDataParams.map { formDataParams =>
            if (formDataNeedsMultipart) {
              q"""_multipart"""
            } else {
              q"""UrlForm($formDataParams.collect({ case (n, Some(v)) => (n, v) }): _*)"""
            }
          }

          val (tracingExpr, httpClientName) =
            if (tracing)
              (List(q"""val tracingHttpClient = traceBuilder(s"$${clientName}:$${methodName}")(httpClient)"""), q"tracingHttpClient")
            else
              (List(), q"httpClient")
          val multipartExpr =
            formDataParams
              .filter(_ => formDataNeedsMultipart)
              .map(formDataParams => q"""val _multipart = Multipart($formDataParams.flatten.toVector)""")
          val headersExpr = if (formDataNeedsMultipart) {
            List(q"val allHeaders = headers ++ $headerParams ++ _multipart.headers")
          } else {
            List(q"val allHeaders = headers ++ $headerParams")
          }
          val req = q"Request[F](method = Method.${Term.Name(httpMethod.toString.toUpperCase)}, uri = ${urlWithParams}, headers = Headers(allHeaders))"
          val reqWithBody = formEntity
            .map(e => q"$req.withEntity($e)")
            .orElse(safeBody.map(_._1).map(e => q"$req.withEntity($e)(${Term.Name(s"${methodName}Encoder")})"))
            .getOrElse(req)
          val reqExpr = List(
            q"val req = $reqWithBody"
          )
          val responseCompanionTerm = Term.Name(s"${methodName.capitalize}Response")
          val cases = responses.value.map { resp =>
            val responseTerm = Term.Name(s"${resp.statusCodeName.value}")
            resp.value.fold[Case](
              p"case ${resp.statusCodeName}(_) => effect.pure($responseCompanionTerm.$responseTerm)"
            ) { _ =>
              p"case ${resp.statusCodeName}(resp) => ${Term.Name(s"$methodName${resp.statusCodeName}Decoder")}.decode(resp, strict = false).fold(throw _, identity).map($responseCompanionTerm.$responseTerm)"
            }
          } :+ p"case resp => effect.raiseError(UnexpectedStatus(resp.status))"
          // Get the response type
          val responseTypeRef = Type.Name(s"${methodName.capitalize}Response")
          val executeReqExpr  = List(q"""$httpClientName.fetch(req)(${Term.PartialFunction(cases)})""")
          val methodBody: Term =
            q"""
            {
              ..${tracingExpr ++ multipartExpr ++ headersExpr ++ reqExpr ++ executeReqExpr}
            }
            """

          val formParams = formArgs.map(
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

          val arglists: List[List[Term.Param]] = List(
            Some(
              (tracingArgsPre.map(_.param) ++ pathArgs.map(_.param) ++ qsArgs
                .map(_.param) ++ formParams ++ body
                .map(_.param) ++ headerArgs.map(_.param) ++ tracingArgsPost
                .map(_.param)) :+ defaultHeaders
            ),
            implicitParams
          ).flatten

          RenderedClientOperation[ScalaLanguage](
            q"""
              def ${Term
              .Name(methodName)}(...${arglists}): F[$responseTypeRef] = $methodBody
            """,
            generateCodecs(methodName, body, responses, produces, consumes)
          )
        }

        for {
          // Placeholder for when more functions get logging
          _ <- Target.pure(())

          consumes = operation.consumes
          produces = operation.produces

          headerArgs = parameters.headerParams
          pathArgs   = parameters.pathParams
          qsArgs     = parameters.queryStringParams
          bodyArgs   = parameters.bodyParams
          formArgs   = parameters.formParams

          _ <- Target.log.debug("generateClientOperation")(s"pathArgs: ${pathArgs}")

          // Generate the url with path, query parameters
          urlWithParams <- generateUrlWithParams(pathStr, pathArgs, qsArgs)

          _ <- Target.log.debug("generateClientOperation")(s"Generated: ${urlWithParams}")
          // Generate FormData arguments
          formDataParams = generateFormDataParams(formArgs, consumes.contains("multipart/form-data"))
          // Generate header arguments
          headerParams = generateHeaderParams(headerArgs)

          tracingArgsPre = if (tracing)
            List(ScalaParameter.fromParam(param"traceBuilder: TraceBuilder[F]"))
          else List.empty
          tracingArgsPost = if (tracing)
            List(ScalaParameter.fromParam(param"methodName: String = ${Lit.String(toDashedCase(methodName))}"))
          else List.empty
          extraImplicits = List.empty

          renderedClientOperation = build(methodName, httpMethod, urlWithParams, formDataParams, headerParams, responses, produces, consumes, tracing)(
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

      case GetImports(tracing) => Target.pure(List(q"import org.http4s.Status._"))

      case GetExtraImports(tracing) => Target.pure(List.empty)

      case ClientClsArgs(tracingName, serverUrls, tracing) =>
        val ihc = param"implicit httpClient: Http4sClient[F]"
        val ief = param"implicit effect: Effect[F]"
        Target.pure(
          List(List(formatHost(serverUrls)) ++ (if (tracing)
                                                     Some(formatClientName(tracingName))
                                                   else None),
               List(ief, ihc))
        )

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        Target.pure(Http4sHelper.generateResponseDefinitions(operationId, responses, protocolElems))

      case BuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing) =>
        def extraConstructors(tracingName: Option[String],
                              serverUrls: Option[NonEmptyList[URI]],
                              tpe: Type.Name,
                              ctorCall: Term.New,
                              tracing: Boolean): List[Defn] = {
          val tracingParams: List[Term.Param] = if (tracing) {
            List(formatClientName(tracingName))
          } else {
            List.empty
          }

          List(
            q"""
              def httpClient[F[_]](httpClient: Http4sClient[F], ${formatHost(serverUrls)}, ..${tracingParams})(implicit effect: Effect[F]): ${tpe}[F] = ${ctorCall}
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
            members = List.empty,
            definitions = decls,
            values = List.empty
          )
        )

      case BuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing) =>
        val client =
          q"""
            class ${Type.Name(clientName)}[F[_]](...${ctorArgs}) {
              val basePath: String = ${Lit.String(basePath.getOrElse(""))}

             ..${supportDefinitions};
              ..$clientCalls
            }
          """
        Target.pure(client)
    }

    def generateCodecs(methodName: String,
                       bodyArgs: Option[ScalaParameter[ScalaLanguage]],
                       responses: Responses[ScalaLanguage],
                       produces: Seq[String],
                       consumes: Seq[String]): List[Defn.Val] =
      generateEncoders(methodName, bodyArgs, consumes) ++ generateDecoders(methodName, responses, produces)

    def generateEncoders(methodName: String, bodyArgs: Option[ScalaParameter[ScalaLanguage]], consumes: Seq[String]): List[Defn.Val] =
      bodyArgs.toList.flatMap {
        case ScalaParameter(_, _, _, _, argType) =>
          List(q"val ${Pat.Var(Term.Name(s"${methodName}Encoder"))} = ${Http4sHelper.generateEncoder(argType, consumes)}")
      }

    def generateDecoders(methodName: String, responses: Responses[ScalaLanguage], produces: Seq[String]): List[Defn.Val] =
      for {
        resp <- responses.value
        tpe  <- resp.value.map(_._1)
      } yield q"val ${Pat.Var(Term.Name(s"$methodName${resp.statusCodeName}Decoder"))} = ${Http4sHelper.generateDecoder(tpe, produces)}"
  }

}
