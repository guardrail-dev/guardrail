package dev.guardrail.generators.Scala

// import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import cats.Monad
// import cats.data.Ior
import cats.data.NonEmptyList
// import dev.guardrail.SwaggerUtil
import dev.guardrail.{ RenderedClientOperation, StaticDefns, StrictProtocolElems, SupportDefinition, Target }
import dev.guardrail.generators.LanguageParameters
// import dev.guardrail.generators.{ LanguageParameter, LanguageParameters, RawParameterName }
// import dev.guardrail.generators.syntax.Scala._
// import dev.guardrail.generators.syntax._
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.Responses
// import dev.guardrail.protocol.terms.{ ContentType, MultipartFormData, Responses, TextPlain }
import dev.guardrail.protocol.terms.client._
import dev.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }
// import dev.guardrail.shims._
import java.net.URI
// import scala.meta._

object EndpointsClientGenerator {
  def ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ClientTerms[ScalaLanguage, Target] =
    new ClientTermInterp
  class ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ClientTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

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
    ): Target[RenderedClientOperation[ScalaLanguage]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")

    def getImports(tracing: Boolean): Target[List[scala.meta.Import]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")

    def getExtraImports(tracing: Boolean): Target[List[scala.meta.Import]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")

    def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): Target[List[List[scala.meta.Term.Param]]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")

    def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[ScalaLanguage],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]]
    ): Target[List[scala.meta.Defn]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")

    def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ): Target[List[SupportDefinition[ScalaLanguage]]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")

    def buildStaticDefns(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        ctorArgs: List[List[scala.meta.Term.Param]],
        tracing: Boolean
    ): Target[StaticDefns[ScalaLanguage]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")

    def buildClient(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        basePath: Option[String],
        ctorArgs: List[List[scala.meta.Term.Param]],
        clientCalls: List[scala.meta.Defn],
        supportDefinitions: List[scala.meta.Defn],
        tracing: Boolean
    ): Target[NonEmptyList[Either[scala.meta.Defn.Trait, scala.meta.Defn.Class]]] =
      Target.raiseUserError("endpoints client generation is not currently supported (last supported: 0.64.1)")
  }
}
/*
object EndpointsClientGenerator {
  def ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ClientTerms[ScalaLanguage, Target] = new ClientTermInterp
  class ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ClientTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

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

      def generateFormDataParams(parameters: List[LanguageParameter[ScalaLanguage]], needsMultipart: Boolean): Option[Term] =
        if (parameters.isEmpty) {
          None
        } else if (needsMultipart) {
          def liftOptionFileTerm(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.map(v => (${tName.toLit}, Right(v)))"

          def liftFileTerm(tParamName: Term, tName: RawParameterName) =
            q"Some((${tName.toLit}, Right($tParamName)))"

          def liftOptionTerm(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.map(v => (${tName.toLit}, Left(Formatter.show(v))))"

          def liftTerm(tParamName: Term, tName: RawParameterName) =
            q"Some((${tName.toLit}, Left(Formatter.show($tParamName))))"

          val lifter: Term.Param => (Term, RawParameterName) => Term = {
            case param"$_: Option[org.scalajs.dom.raw.File]"      => liftOptionFileTerm _
            case param"$_: Option[org.scalajs.dom.raw.File] = $_" => liftOptionFileTerm _
            case param"$_: org.scalajs.dom.raw.File"              => liftFileTerm _
            case param"$_: org.scalajs.dom.raw.File = $_"         => liftFileTerm _
            case param"$_: Option[$_]"                            => liftOptionTerm _
            case param"$_: Option[$_] = $_"                       => liftOptionTerm _
            case _                                                => liftTerm _
          }

          val args: List[Term] = parameters.map {
            case LanguageParameter(_, param, paramName, argName, _) =>
              lifter(param)(paramName, argName)
          }
          Some(q"List[Option[(String, Either[String, org.scalajs.dom.raw.File])]](..${args}).flatten")
        } else {
          def liftTerm(tParamName: Term, tName: RawParameterName) =
            q"List((${tName.toLit}, Formatter.show($tParamName)))"

          def liftIterable(tParamName: Term, tName: RawParameterName) =
            q"$tParamName.toList.map((${tName.toLit}, _))"

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
          Some(q"List(..$args).flatten")
        }

      def generateHeaderParams(parameters: List[LanguageParameter[ScalaLanguage]]): Term = {
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
          case LanguageParameter(_, param, paramName, argName, _) =>
            lifter(param)(paramName, argName)
        }
        q"scala.collection.immutable.Seq[Option[String]](..$args).flatten"
      }

      def build(
          methodName: String,
          responseClsName: String,
          httpMethod: HttpMethod,
          pathPattern: Term,
          formDataParams: Option[Term],
          staticQueryParams: Option[Term],
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
          bodyArgs: Option[LanguageParameter[ScalaLanguage]],
          headerArgs: List[LanguageParameter[ScalaLanguage]],
          extraImplicits: List[Term.Param]
      ): RenderedClientOperation[ScalaLanguage] = {

        val implicitParams = Option(extraImplicits).filter(_.nonEmpty)

        val (fallbackBodyAlgebra, fallbackBodyArgument) = (Some(q"emptyRequest"), None)

        val (textPlainAlgebra, textPlainArgument): (Option[Term], Option[Term]) =
          if (consumes.contains(TextPlain))
            (bodyArgs.map(_ => q"textPlainRequest"), bodyArgs.map(sp => if (sp.required) sp.paramName else q"""${sp.paramName}.getOrElse("")"""))
          else (None, None)

        val (formAlgebra, formArgument): (Option[Term], Option[Term]) = {
          (
            formDataParams.map(
              _ =>
                if (consumes.contains(MultipartFormData)) {
                  q"multipartFormDataRequest"
                } else {
                  q"formDataRequest[List[(String, String)]]()"
                }
            ),
            formDataParams
          )
        }

        val (tracingExpr, httpClientName) =
          if (tracing)
            (List(q"""val tracingHttpClient = traceBuilder(s"$${clientName}:$${methodName}")(httpClient)"""), q"tracingHttpClient")
          else
            (List.empty, q"httpClient")

        val urlWithParams = Ior.fromOptions(NonEmptyList.fromList(qsArgs), staticQueryParams).fold(pathPattern) {
          _.leftMap({ nel =>
            val (_, components) =
              nel.traverse(x => (List(x.argType), q"showQs[${x.argType}](${x.argName.toLit})")).map(_.reduceLeft[Term]((a, b) => q"$a & $b"))
            components
          }).fold(
            components => q"$pathPattern /? $components",
            staticArgs => q"$pathPattern /? $staticArgs",
            (components, staticArgs) => q"$pathPattern /? ($components & $staticArgs)"
          )
        }

        val bodyAlgebra: Option[Term] = formAlgebra
          .orElse(textPlainAlgebra)
          .orElse(bodyArgs.map(x => q"jsonRequest[${x.argType}]"))
          .orElse(fallbackBodyAlgebra)

        val bodyArgument: Option[Term] = formArgument
          .orElse(textPlainArgument)
          .orElse(bodyArgs.map(x => x.paramName))
          .orElse(fallbackBodyArgument)

        val (tracingAlgebra, tracingArgument): (Option[Term], Option[Term]) =
          if (tracing) {
            (Some(q"tracerHeader"), Some(q"Map.empty"))
          } else {
            (None, None)
          }

        val responseCompanionTerm = Term.Name(responseClsName)
        val responseCompanionType = Type.Name(responseClsName)

        val cases = responses.value.map { resp =>
            val responseTerm = Term.Name(s"${resp.statusCodeName.value}")
            (resp.value, resp.headers.value) match {
              case (None, Nil) =>
                p"""case ${Lit.Int(resp.statusCode)} =>
                  (_: XMLHttpRequest) => Right($responseCompanionTerm.$responseTerm)
                """
              case (None, headers) =>
                val params = headers.map { header =>
                  val lit  = Lit.String(header.name)
                  val expr = q"xhr.getResponseHeader($lit)"
                  if (header.isRequired) {
                    expr
                  } else {
                    q"Option($expr)"
                  }
                }
                p"""case ${Lit.Int(resp.statusCode)} =>
                  (_: XMLHttpRequest) => Right($responseCompanionTerm.$responseTerm(..$params))
                """
              case (Some((_, tpe, _)), headers) =>
                val params = Term.Name("v") :: headers.map { header =>
                        val lit  = Lit.String(header.name)
                        val expr = q"xhr.getResponseHeader($lit)"
                        if (header.isRequired) {
                          expr
                        } else {
                          q"Option($expr)"
                        }
                      }
                p"""case ${Lit.Int(resp.statusCode)} =>
                  (_: XMLHttpRequest) => parser.parse(xhr.responseText).flatMap(CirceDecoder[${tpe}].decodeJson _).map(v => $responseCompanionTerm.$responseTerm(..$params))
                """
            }
          } :+ p"case _ => (_: XMLHttpRequest) => Left(new UnknownStatusException(xhr))"

        val responseTypeRef = Type.Name(s"${methodName.capitalize}Response")

        val methodParameters: List[List[Term.Param]] = List(
          Some(
            tracingArgsPre.map(_.param) ++ pathArgs.map(_.param) ++ qsArgs
                  .map(_.param) ++ formArgs.map(_.param) ++ bodyArgs
                  .map(_.param) ++ headerArgs.map(_.param) ++ tracingArgsPost
                  .map(_.param)
          ),
          implicitParams
        ).flatten

        val endpointCallArgs = {
          def adaptNel: NonEmptyList[Term] => Term = {
            case NonEmptyList(a, Nil)     => a
            case NonEmptyList(a, x :: xs) => emulateTupler(a, NonEmptyList(x, xs))
          }
          @scala.annotation.tailrec
          def emulateTupler(base: Term, xs: NonEmptyList[Term]): Term.Tuple = xs match {
            case NonEmptyList(x, Nil) =>
              base match {
                case q"($a, $b, $c)" => q"($a, $b, $c, $x)"
                case q"($a, $b)"     => q"($a, $b, $x)"
                case as              => q"($as, $x)"
              }
            case NonEmptyList(x, y :: rest) =>
              val next = base match {
                case q"($a, $b, $c)" => q"($a, $b, $c, $x)"
                case q"($a, $b)"     => q"($a, $b, $x)"
                case as              => q"($as, $x)"
              }
              emulateTupler(next, NonEmptyList(y, rest))
          }

          def hackyFoldLimitedTupleTree: NonEmptyList[Term] => NonEmptyList[Term] = {
            case NonEmptyList(x, xs) =>
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
          val hostPart: List[Term]    = List(q"host", q"basePath")
          val pathPart: List[Term]    = hostPart ++ pathArgs.map(_.paramName)
          val qsPart: List[Term]      = qsArgs.map(_.paramName)
          val bodyPart: List[Term]    = bodyArgument.toList
          val headerPart: List[Term]  = headerArgs.map(_.paramName)
          val tracingPart: List[Term] = tracingArgument.toList

          val res0: Option[NonEmptyList[NonEmptyList[Term]]] =
            NonEmptyList.fromList(
              List[List[Term]](
                pathPart,
                qsPart,
                bodyPart,
                headerPart ++ tracingPart
              ).flatMap(NonEmptyList.fromList(_))
            )

          res0.fold[Term](q"()")({ nel =>
            val res @ NonEmptyList(h, t) = nel.map(hackyFoldLimitedTupleTree)
            t.foldLeft(adaptNel(h))(emulateTupler(_, _))
          })
        }

        val algebraParams = List[Option[Term]](
          Some(urlWithParams),
          bodyAlgebra,
          Option(q"None"), // Documentation
          NonEmptyList
            .fromList(headerArgs)
            .map(_.map[Term]({ arg =>
              arg.argType match {
                case t"Option[$tpe]" => q"""showOptHeader[${tpe}](${arg.argName.toLit}, None)"""
                case tpe             => q"""showHeader[${tpe}](${arg.argName.toLit}, None)"""
              }
            }))
            .fold[Option[NonEmptyList[Term]]](tracingAlgebra.map(NonEmptyList(_, Nil)))({ xs =>
              Some(xs ++ tracingAlgebra.toList)
            })
            .map(_.reduceLeft((a, b) => q"$a ++ $b"))
        ).flatten

        val endpointDefinition = q"""
              endpoint(request(..${Term.Name(httpMethod.toString.toLowerCase.capitalize) :: algebraParams}), ${Term.Name(s"${methodName}ResponseMapper")})
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
              def ${Term.Name(s"${methodName}ResponseMapper")}: Response[$responseCompanionType] = xhr => Some(${Term
              .Match(q"xhr.status", cases)})
              """,
            q"""
                val ${Pat.Var(Term.Name(s"${methodName}Endpoint"))} = $endpointDefinition
              """
          )
        )
      }

      Target.log.function("generateClientOperation")(
        for {
          // Placeholder for when more functions get logging
          _ <- Target.pure(())

          produces = operation.get.produces.toList.flatMap(ContentType.unapply(_))
          consumes = operation.get.consumes.toList.flatMap(ContentType.unapply(_))

          headerArgs = parameters.headerParams
          pathArgs   = parameters.pathParams
          qsArgs     = parameters.queryStringParams
          bodyArgs   = parameters.bodyParams
          formArgs   = parameters.formParams

          _ <- Target.log.debug(s"pathArgs: $pathArgs")

          // Generate the url with path, query parameters
          urlWithPathParams <- SwaggerUtil.paths.generateUrlEndpointsPathExtractors(pathStr, pathArgs)
          (pathPattern, staticQueryParams) = urlWithPathParams

          // _ <- Target.log.debug(s"Generated: $urlWithParams")
          // Generate FormData arguments
          formDataParams = generateFormDataParams(formArgs, consumes.contains(MultipartFormData))
          // Generate header arguments
          headerParams = generateHeaderParams(headerArgs)

          tracingArgsPre = if (tracing)
            List(LanguageParameter.fromParam(param"traceBuilder: TraceBuilder"))
          else List.empty
          tracingArgsPost = if (tracing)
            List(LanguageParameter.fromParam(param"methodName: String = ${Lit.String(methodName.toDashedCase)}"))
          else List.empty
          extraImplicits = List.empty
          renderedClientOperation = build(
            methodName,
            responseClsName,
            httpMethod,
            pathPattern,
            formDataParams,
            staticQueryParams,
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
        } yield renderedClientOperation
      )
    }
    def getImports(tracing: Boolean): Target[List[scala.meta.Import]]      = Target.pure(List.empty)
    def getExtraImports(tracing: Boolean): Target[List[scala.meta.Import]] = Target.pure(List.empty)
    def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): Target[List[List[scala.meta.Term.Param]]] =
      Target.pure(List(List(formatHost(serverUrls)) ++ (if (tracing) Some(formatClientName(tracingName)) else None)))
    def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[ScalaLanguage],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]]
    ): Target[List[scala.meta.Defn]] =
      Target.pure(Http4sHelper.generateResponseDefinitions(responseClsName, responses, protocolElems))
    def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ): Target[List[SupportDefinition[ScalaLanguage]]] =
      Target.pure(List.empty)
    def buildStaticDefns(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        ctorArgs: List[List[scala.meta.Term.Param]],
        tracing: Boolean
    ): Target[StaticDefns[ScalaLanguage]] = {
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
            new ${Type.Name(clientName)}(...${paramsToArgs(ctorArgs)})
          """
      }

      val definitions: List[Defn] = List(
        q"def apply(...${ctorArgs}): ${Type.Name(clientName)} = ${ctorCall}"
      )

      Target.pure(StaticDefns[ScalaLanguage](clientName, List.empty, definitions))
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
            trait ${Type.Name(s"${clientName}Algebra")} extends algebra.Endpoints with algebra.circe.JsonEntitiesFromCodecs with AddPathSegments with FormData {
              ..${responseDefs.map {
          case q"def $name: Response[$tpe] = $_" => q"def $name: Response[$tpe]"
        }};
              ..${endpointDefs}
            }
         """
      val client =
        q"""
            class ${Type
          .Name(clientName)}(...$ctorArgs) extends ${Init(Type.Name(s"${clientName}Algebra"), Name(""), Nil)} with xhr.JsonEntitiesFromCodecs with xhr.faithful.Endpoints with XhrAddPathSegments with XhrFormData {
              val basePath: Option[String] = ${basePath.fold[Term](q"Option.empty[String]")(bp => q"Option(${Lit.String(bp)})")}

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
 */
