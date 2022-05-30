package dev.guardrail.generators.scala.http4s

import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import cats.Traverse
import scala.meta._
import scala.reflect.runtime.universe.typeTag

import dev.guardrail.{ AuthImplementation, Target, UserError }
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.core.Tracker
import dev.guardrail.core.extract.{ ServerRawResponse, TracingLabel }
import dev.guardrail.generators.{ CustomExtractionField, LanguageParameter, LanguageParameters, RenderedRoutes, TracingField }
import dev.guardrail.generators.syntax._
import dev.guardrail.generators.operations.TracingLabelFormatter
import dev.guardrail.generators.scala.syntax._
import dev.guardrail.generators.scala.{ CirceModelGenerator, ModelGeneratorType, ResponseADTHelper }
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.spi.{ ModuleLoadResult, ServerGeneratorLoader }
import dev.guardrail.terms.{ ContentType, Header, Response, Responses }
import dev.guardrail.terms.server._
import dev.guardrail.shims._
import dev.guardrail.terms.{ RouteMeta, SecurityScheme }

import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import _root_.io.swagger.v3.oas.models.Operation
import dev.guardrail.terms.SecurityRequirements
import dev.guardrail.AuthImplementation.Disable
import dev.guardrail.AuthImplementation.Native
import dev.guardrail.AuthImplementation.Simple
import dev.guardrail.AuthImplementation.Custom

class Http4sServerGeneratorLoader extends ServerGeneratorLoader {
  type L = ScalaLanguage
  override def reified = typeTag[Target[ScalaLanguage]]
  val apply            = ModuleLoadResult.buildFrom(ModuleLoadResult.extract(Http4sVersion.unapply))(http4sVersion => Http4sServerGenerator(http4sVersion))
}

object Http4sServerGenerator {
  def apply(version: Http4sVersion): ServerTerms[ScalaLanguage, Target] =
    new Http4sServerGenerator(version)

  def generateUrlPathExtractors(
      path: Tracker[String],
      pathArgs: List[LanguageParameter[ScalaLanguage]],
      modelGeneratorType: ModelGeneratorType
  ): Target[(Pat, Option[Pat])] =
    for {
      (parts, (trailingSlash, queryParams)) <- Http4sPathExtractor.runParse(path, pathArgs, modelGeneratorType)
      (directive, bindings) = parts
        .foldLeft[(Pat, List[Term.Name])]((p"${Term.Name("Root")}", List.empty)) { case ((acc, bindings), (termName, c)) =>
          (p"$acc / ${c}", bindings ++ termName)
        }
      trailingSlashed =
        if (trailingSlash) {
          p"$directive / ${Lit.String("")}"
        } else directive
    } yield (trailingSlashed, queryParams)
}

class Http4sServerGenerator private (version: Http4sVersion) extends ServerTerms[ScalaLanguage, Target] {

  val customExtractionTypeName: Type.Name = Type.Name("E")
  val authContextTypeName: Type.Name      = Type.Name("AuthContext")
  val authErrorTypeName: Type.Name        = Type.Name("AuthError")
  val authSchemesTypeName: Type.Name      = Type.Name("AuthSchemes")
  val authRequirementTypeName: Type.Name  = Type.Name("AuthRequirement")

  private val bodyUtf8Decode = version match {
    case Http4sVersion.V0_22 => q"utf8Decode"
    case Http4sVersion.V0_23 => q"utf8.decode"
  }

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
    Target.pure(ResponseADTHelper.generateResponseDefinitions(responseClsName, responses, protocolElems))

  def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean) =
    for {
      _ <- Target.log.debug(s"buildCustomExtractionFields(${operation.unwrapTracker.showNotNull}, ${resourceName}, ${customExtraction})")
      res <-
        if (customExtraction) {
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
      res <-
        if (tracing) {
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

  override def generateRoutes(
      tracing: Boolean,
      resourceName: String,
      handlerName: String,
      basePath: Option[String],
      routes: List[GenerateRouteMeta[ScalaLanguage]],
      protocolElems: List[StrictProtocolElems[ScalaLanguage]],
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
      securityExposure: SecurityExposure,
      authImplementation: AuthImplementation
  ): Target[RenderedRoutes[ScalaLanguage]] =
    for {
      renderedRoutes <- routes
        .traverse(generateRoute(resourceName, basePath, securityExposure, authImplementation))
        .map(_.flatten)
      routeTerms = renderedRoutes.map(_.route)
      combinedRouteTerms <- combineRouteTerms(routeTerms)
      methodSigs = renderedRoutes.map(_.methodSig)
      securitySchemesDefinitions <-
        if (authImplementation == AuthImplementation.Disable || authImplementation == AuthImplementation.Native)
          Target.pure(List.empty)
        else generateSecuritySchemes(securitySchemes, routes.flatMap(_.routeMeta.securityRequirements), authImplementation)
    } yield RenderedRoutes[ScalaLanguage](
      List(combinedRouteTerms),
      List.empty,
      methodSigs,
      renderedRoutes
        .flatMap(_.supportDefinitions)
        .groupBy(_.structure)
        .flatMap(_._2.headOption)
        .toList
        .sortBy(_.toString()), // Only unique supportDefinitions by structure
      renderedRoutes.flatMap(_.handlerDefinitions),
      securitySchemesDefinitions
    )

  private def generateSecuritySchemes(
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
      securityRequirements: List[SecurityRequirements],
      authImplementation: AuthImplementation
  ): Target[List[scala.meta.Defn]] = {
    val schemesNames = securitySchemes.keySet
    val uniqueRequirements: List[Tracker[String]] =
      securityRequirements.flatMap(_.requirements.flatMap(_.map(_.keys.toNonEmptyList).indexedDistribute).toList).distinctBy(_.unwrapTracker)

    val errorTermName = Term.Name(authErrorTypeName.value)
    val (simpleAuthErrors, simpleAuthenticator) = if (authImplementation == AuthImplementation.Simple) {
      val errorInit = Init(authErrorTypeName, Name(""), List.empty)
      val authErrors = List(
        q"""sealed trait $authErrorTypeName""",
        q"""
          object $errorTermName {
            final case object Unauthorized extends $errorInit
            final case object Forbidden extends $errorInit
          }
        """
      )

      val authenticator = List(q"""
      def authenticate[F[_]: _root_.cats.Monad, ${tparam"$authContextTypeName"}](
        middleware: ($authSchemesTypeName, Set[String], Request[F]) => F[Either[$authErrorTypeName, $authContextTypeName]],
        schemes: _root_.cats.data.NonEmptyList[_root_.cats.data.NonEmptyMap[$authSchemesTypeName, Set[String]]],
        req: Request[F]
      ): F[Either[$authErrorTypeName, $authContextTypeName]] = {
        schemes.foldM[F, Either[$authErrorTypeName, $authContextTypeName]](Left($errorTermName.Unauthorized)){ case (result, el) =>
          result match {
            case Left(value) =>
              val nel = el.toNel
              val (headScheme, headScopes) = nel.head
              val headResult = middleware(headScheme, headScopes, req)

              nel.tail.foldLeft(headResult)({ case (acc , (scheme, scopes)) =>
                acc.flatMap {
                  case Right(_) => middleware(scheme, scopes, req)
                  case l: Left[$authErrorTypeName, $authContextTypeName] => _root_.cats.Applicative[F].pure(l)
                }
              })
            case r: Right[$authErrorTypeName, $authContextTypeName] => _root_.cats.Applicative[F].pure(r)
          }
        }
      }
    """)

      (authErrors, authenticator)
    } else (List.empty, List.empty)

    uniqueRequirements
      .traverse { reqName =>
        val existanceCheck =
          if (schemesNames.contains(reqName.unwrapTracker)) Target.pure(())
          else Target.log.warning(s"Security requirement '${reqName.unwrapTracker}' is missing in security schemes (${reqName.showHistory})")

        existanceCheck *> Target.pure(q"""
          case object ${securitySchemeNameToClassName(reqName.unwrapTracker)} extends ${Init(authSchemesTypeName, Name(""), List.empty)} {
            override val name: String = ${Lit.String(reqName.unwrapTracker)}
          }
        """)
      }
      .map {
        case list if list.nonEmpty =>
          List(
            q"""sealed trait $authSchemesTypeName {
              def name: String
            }""",
            q"""object ${Term.Name(authSchemesTypeName.value)} {
              implicit val order: _root_.cats.kernel.Order[$authSchemesTypeName] = _root_.cats.kernel.Order.by(_.name)

              sealed trait $authRequirementTypeName
              object ${Term.Name(authRequirementTypeName.value)} {
                case object Required extends ${Init(authRequirementTypeName, Name(""), List.empty)}
                case object Optional extends ${Init(authRequirementTypeName, Name(""), List.empty)}
              }

              ..$list
            }
            """
          ) ::: simpleAuthErrors ::: simpleAuthenticator
        case list => list
      }
  }

  private def securitySchemeNameToClassName(name: String): Term.Name =
    Term.Name(name.toPascalCase)

  override def renderHandler(
      handlerName: String,
      methodSigs: List[scala.meta.Decl.Def],
      handlerDefinitions: List[scala.meta.Stat],
      responseDefinitions: List[scala.meta.Defn],
      customExtraction: Boolean,
      authImplementation: AuthImplementation,
      securityExposure: SecurityExposure
  ) =
    Target.log.function("renderHandler")(for {
      _ <- Target.log.debug(s"Args: ${handlerName}, ${methodSigs}")
      extractType = if (customExtraction) List(tparam"-$customExtractionTypeName") else List.empty
      authType =
        if (authImplementation == Native || (authImplementation != Disable && securityExposure != SecurityExposure.Undefined)) {
          List(tparam"$authContextTypeName")
        } else List.empty
      tParams = List(tparam"F[_]") ++ extractType ++ authType
    } yield q"""
    trait ${Type.Name(handlerName)}[..$tParams] {
      ..${methodSigs ++ handlerDefinitions}
    }
  """)

  def getExtraRouteParams(
      resourceName: String,
      customExtraction: Boolean,
      tracing: Boolean,
      authImplementation: AuthImplementation,
      securityExposure: SecurityExposure
  ) =
    Target.log.function("getExtraRouteParams")(for {
      _ <- Target.log.debug(s"getExtraRouteParams(${tracing}, ${authImplementation})")
      mapRoute = {
        val requestType = authImplementation match {
          case Native => t"AuthedRequest[F, $authContextTypeName]"
          case _      => t"Request[F]"
        }
        param"""mapRoute: (String, $requestType, F[Response[F]]) => F[Response[F]] = (_: String, _: $requestType, r: F[Response[F]]) => r"""
      }
      customExtraction_ =
        if (customExtraction) {
          Option(param"""customExtract: String => Request[F] => $customExtractionTypeName""")
        } else Option.empty
      tracing_ =
        if (tracing) {
          Option(param"""trace: String => Request[F] => TraceBuilder[F]""")
        } else Option.empty
      resourceTerm = Term.Name(resourceName)
      authentication_ = authImplementation match {
        case Simple if securityExposure != SecurityExposure.Undefined =>
          val authType = Type.Select(resourceTerm, authSchemesTypeName)
          Option(
            param"""authenticationMiddleware: ($authType, Set[String], Request[F]) => F[Either[${resourceTerm}.$authErrorTypeName, $authContextTypeName]]"""
          )
        case Custom if securityExposure == SecurityExposure.Required =>
          Option(
            param"""authenticationMiddleware: (_root_.cats.data.NonEmptyList[_root_.cats.data.NonEmptyMap[${resourceTerm}.$authSchemesTypeName, Set[String]]], Request[F]) => F[$authContextTypeName]"""
          )
        case Custom if securityExposure == SecurityExposure.Optional =>
          val authRequirementType = Type.Select(Term.Select(resourceTerm, Term.Name(authSchemesTypeName.value)), authRequirementTypeName)
          Option(
            param"""authenticationMiddleware: (_root_.cats.data.NonEmptyList[_root_.cats.data.NonEmptyMap[${resourceTerm}.$authSchemesTypeName, Set[String]]], ${authRequirementType}, Request[F]) => F[$authContextTypeName]"""
          )
        case _ => Option.empty
      }
    } yield customExtraction_.toList ::: tracing_.toList ::: authentication_.toList ::: List(mapRoute))

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
      securitySchemesDefinitions: List[scala.meta.Defn],
      customExtraction: Boolean,
      authImplementation: AuthImplementation
  ): Target[List[Defn]] =
    Target.log.function("renderClass")(
      for {
        _ <- Target.log.debug(s"Args: ${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams}")
        extractType     = List(customExtractionTypeName).map(x => tparam"$x").filter(_ => customExtraction)
        authType        = List(tparam"$authContextTypeName").filter(_ => securitySchemesDefinitions.nonEmpty || authImplementation == AuthImplementation.Native)
        resourceTParams = List(tparam"F[_]") ++ extractType ++ authType
        handlerTParams = List(Type.Name("F")) ++
          List(customExtractionTypeName).filter(_ => customExtraction) ++
          List(authContextTypeName).filter(_ => securitySchemesDefinitions.nonEmpty || authImplementation == AuthImplementation.Native)
        routesParams = List(param"handler: ${Type.Name(handlerName)}[..$handlerTParams]")
        routesDefinition = authImplementation match {
          case Native => q"""
            def routes(..${routesParams}): AuthedRoutes[$authContextTypeName, F] = AuthedRoutes.of {
                ..${combinedRouteTerms}
              }
          """
          case _ => q"""
            def routes(..${routesParams}): HttpRoutes[F] = HttpRoutes.of {
                ..${combinedRouteTerms}
              }
          """
        }
      } yield List(
        q"""
          class ${Type.Name(resourceName)}[..$resourceTParams](..$extraRouteParams)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
            import ${Term.Name(resourceName)}._

            ..${supportDefinitions};
            $routesDefinition
          }
        """,
        q"""object ${Term.Name(resourceName)} {
            ..${securitySchemesDefinitions}

            ..${responseDefinitions}
        }"""
      )
    )

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
      case ""        => Target.pure((p"${Term.Name("Root")}", None))
      case finalPath => Http4sServerGenerator.generateUrlPathExtractors(Tracker.cloneHistory(path, finalPath), pathArgs, CirceModelGenerator.V012)
    }

  def directivesFromParams[T](
      required: LanguageParameter[ScalaLanguage] => Type => Target[T],
      multi: LanguageParameter[ScalaLanguage] => Type => (Term => Term) => Target[T],
      multiOpt: LanguageParameter[ScalaLanguage] => Type => (Term => Term) => Target[T],
      optional: LanguageParameter[ScalaLanguage] => Type => Target[T]
  )(params: List[LanguageParameter[ScalaLanguage]]): Target[List[T]] =
    for {
      directives <- params.traverse[Target, T] { case scalaParam @ LanguageParameter(_, param, _, _, argType) =>
        val containerTransformations = Map[String, Term => Term](
          "Iterable"   -> identity _,
          "List"       -> (term => q"$term.toList"),
          "Vector"     -> (term => q"$term.toVector"),
          "Seq"        -> (term => q"$term.toSeq"),
          "IndexedSeq" -> (term => q"$term.toIndexedSeq")
        )

        val transform: Option[LanguageParameter[ScalaLanguage] => Type => Target[T]] = param match {
          case param"$_: Option[$container[$tpe]]" =>
            containerTransformations.get(container.syntax).map(lift => lp => _ => multiOpt(lp)(tpe)(lift))
          case param"$_: Option[$container[$tpe]] = $_" =>
            containerTransformations.get(container.syntax).map(lift => lp => _ => multiOpt(lp)(tpe)(lift))
          case param"$_: Option[$tpe]" =>
            Some(lp => _ => optional(lp)(tpe))
          case param"$_: Option[$tpe] = $_" =>
            Some(lp => _ => optional(lp)(tpe))
          case param"$_: $container[$tpe]" =>
            containerTransformations.get(container.syntax).map(lift => lp => _ => multi(lp)(tpe)(lift))
          case param"$_: $container[$tpe] = $_" =>
            containerTransformations.get(container.syntax).map(lift => lp => _ => multi(lp)(tpe)(lift))
          case _ => None
        }

        transform.getOrElse(required).apply(scalaParam)(argType)
      }
    } yield directives

  def bodyToHttp4s(methodName: String, body: Option[LanguageParameter[ScalaLanguage]]): Target[Option[Term => Term]] =
    Target.pure(
      body.map { case LanguageParameter(_, _, paramName, _, _) =>
        content => q"""
                req
                  .attemptAs(${Term.Name(s"${methodName.uncapitalized}Decoder")})
                  .foldF(
                    err =>
                      err.cause match {
                        case Some(circeErr: io.circe.DecodingFailure) =>
                          Response[F](
                          status = org.http4s.Status.UnprocessableEntity,
                          body   = stringEncoder.toEntity("The request body was invalid. " + circeErr.message + ": " + circeErr.history.mkString(", ")).body
                        ).pure[F]
                        case _ => err.toHttpResponse[F](req.httpVersion).pure[F]
                      },
                  ${param"$paramName"} => $content
                  )
              """
      }
    )

  case class Param(generator: Option[Enumerator.Generator], matcher: Option[(Term, Pat)], handlerCallArg: Term)

  def headersToHttp4s: List[LanguageParameter[ScalaLanguage]] => Target[List[Param]] =
    directivesFromParams(
      arg => {
        case t"String" =>
          Target.pure(
            Param(None, Some((q"req.headers.get(CIString(${arg.argName.toLit})).map(_.head.value)", p"Some(${Pat.Var(arg.paramName)})")), arg.paramName)
          )
        case tpe =>
          Target.pure(
            Param(
              None,
              Some(
                (
                  q"req.headers.get(CIString(${arg.argName.toLit})).map(_.head.value).map(Json.fromString(_).as[$tpe])",
                  p"Some(Right(${Pat.Var(arg.paramName)}))"
                )
              ),
              arg.paramName
            )
          )
      },
      arg => _ => _ => Target.raiseUserError(s"Unsupported Iterable[${arg}"),
      arg => _ => _ => Target.raiseUserError(s"Unsupported Option[Iterable[${arg}]]"),
      arg => {
        case t"String" => Target.pure(Param(None, None, q"req.headers.get(CIString(${arg.argName.toLit})).map(_.head.value)"))
        case tpe =>
          Target.pure(
            Param(
              None,
              Some(
                (
                  q"req.headers.get(CIString(${arg.argName.toLit})).map(_.head.value).map(Json.fromString(_).as[$tpe]).sequence",
                  p"Right(${Pat.Var(arg.paramName)})"
                )
              ),
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
          lift => Target.pure(Param(None, Some((q"urlForm.values.get(${arg.argName.toLit})", p"Some(${Pat.Var(arg.paramName)})")), lift(q"${arg.paramName}")))
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
        case t"String" => lift => Target.pure(Param(None, None, q"urlForm.values.get(${arg.argName.toLit}).map(${lift(q"_")})"))
        case tpe =>
          lift =>
            Target.pure(
              Param(
                None,
                Some(
                  (
                    q"${lift(q"urlForm.values.get(${arg.argName.toLit})")}.flatMap(${lift(q"_")}).map(Json.fromString(_).as[$tpe]).sequence.sequence",
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
                      enumerator"${Pat.Var(Term.Name(s"${arg.argName.value}Option"))} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid).sequence"
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
                      enumerator"${Pat.Var(Term.Name(s"${arg.argName.value}Option"))} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence"
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
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid).sequence"
                      ),
                      None,
                      arg.paramName
                    )
                  )
                case tpe =>
                  Target.pure(
                    Param(
                      Some(
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence"
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
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid).sequence.map(Option(_).filter(_.nonEmpty))"
                      ),
                      None,
                      arg.paramName
                    )
                  )
                case tpe =>
                  Target.pure(
                    Param(
                      Some(
                        enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.filter(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence.map(Option(_).filter(_.nonEmpty))"
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
                      enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid).sequence"
                    ),
                    None,
                    arg.paramName
                  )
                )
              case tpe =>
                Target.pure(
                  Param(
                    Some(
                      enumerator"${Pat.Var(arg.paramName)} <- multipart.parts.find(_.name.contains(${arg.argName.toLit})).map(_.body.through($bodyUtf8Decode).compile.foldMonoid.flatMap(str => F.fromEither(Json.fromString(str).as[$tpe]))).sequence"
                    ),
                    None,
                    arg.paramName
                  )
                )
            }
    )

  case class RenderedRoute(methodName: String, route: Case, methodSig: Decl.Def, supportDefinitions: List[Defn], handlerDefinitions: List[Stat])

  private def generateRoute(
      resourceName: String,
      basePath: Option[String],
      securityExposure: SecurityExposure,
      authImplementation: AuthImplementation
  ): GenerateRouteMeta[ScalaLanguage] => Target[Option[RenderedRoute]] = {
    case GenerateRouteMeta(
          _,
          methodName,
          responseClsName,
          customExtractionFields,
          tracingFields,
          route,
          parameters,
          responses
        ) =>
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
        val resourceTerm = Term.Name(resourceName)
        val (responseCompanionTerm, responseCompanionType) =
          (Term.Name(responseClsName), Type.Name(responseClsName))
        val responseType = ServerRawResponse(operation)
          .filter(_ == true)
          .fold[Type](t"${resourceTerm}.$responseCompanionType")(Function.const(t"Response[F]"))
        val authContext: Option[(LanguageParameter[ScalaLanguage], Term => Term)] = authImplementation match {
          case Disable => None
          case Native  => Some((LanguageParameter.fromParam(param"authContext: $authContextTypeName"), identity _))
          case Custom =>
            securityRequirements.map { sr =>
              val arg                  = LanguageParameter.fromParam(param"authContext: $authContextTypeName")
              val securityRequirements = renderCustomSecurityRequirements(sr)
              val authContextParam     = param"${arg.paramName}"

              val middlewareArgs = List(securityRequirements) ++ (securityExposure match {
                case SecurityExposure.Optional =>
                  val isRequired = if (sr.optional) q"Optional" else q"Required"
                  Some(
                    Term.Select(
                      Term.Select(Term.Select(resourceTerm, Term.Name(authSchemesTypeName.value)), Term.Name(authRequirementTypeName.value)),
                      isRequired
                    )
                  )
                case _ => None
              }) ++ List(q"req")
              val authTransformer: Term => Term = inner => q"""
                authenticationMiddleware(..${middlewareArgs}).flatMap { $authContextParam =>
                  $inner
                }
              """
              (arg, authTransformer)
            }
          case Simple =>
            securityRequirements.map { sr =>
              val errorTermName = Term.Name(authErrorTypeName.value)
              val inner = if (sr.optional) {
                t"Either[${Type.Singleton(Term.Select(Term.Select(Term.Name(resourceName), errorTermName), q"Forbidden"))}, Option[$authContextTypeName]]"
              } else {
                t"Either[${Term.Name(resourceName)}.$authErrorTypeName, $authContextTypeName]"
              }
              val arg                  = LanguageParameter.fromParam(param"authContext: $inner")
              val securityRequirements = renderCustomSecurityRequirements(sr)
              val authContextParam     = param"${arg.paramName}"

              val authTransformer: Term => Term = inner =>
                if (sr.optional)
                  q"""
                  authenticate[F, $authContextTypeName](authenticationMiddleware, $securityRequirements, req).flatMap {
                    authContextEither =>
                      val ${Pat.Var(Term.Name(authContextParam.name.value))} = authContextEither match {
                        case Right(success) => Right(Option(success))
                        case Left(${Term.Name(authErrorTypeName.value)}.Unauthorized) => Right(Option.empty)
                        case Left(x: ${Type.Singleton(Term.Select(Term.Name(authErrorTypeName.value), q"Forbidden"))}) => Left(x)
                      }
                      $inner
                    }
                    """
                else
                  q"""
                      authenticate[F, $authContextTypeName](authenticationMiddleware, $securityRequirements, req).flatMap {
                        $authContextParam =>
                          $inner
                        }
                        """

              (arg, authTransformer)
            }
        }

        val orderedParameters: List[List[LanguageParameter[ScalaLanguage]]] = List(
          (authContext.map(_._1).toList ++ pathArgs ++ qsArgs ++ bodyArgs ++ formArgs ++ headerArgs).toList
        ) ++
          tracingFields
            .map(_.param)
            .map(List(_)) ++
          customExtractionFields
            .map(_.param)
            .map(List(_))

        val entityProcessor = http4sBody
          .orElse(Some((content: Term) => q"req.decode[UrlForm] { urlForm => $content }").filter(_ => formArgs.nonEmpty && formArgs.forall(!_.isFile)))
          .orElse(Some((content: Term) => q"req.decode[Multipart[F]] { multipart => $content }").filter(_ => formArgs.nonEmpty))
        val fullRouteMatcher = {
          val base = NonEmptyList.fromList(List(additionalQs, http4sQs).flatten).fold(p"$http4sMethod -> $http4sPath") { qs =>
            p"$http4sMethod -> $http4sPath :? ${qs.reduceLeft((a, n) => p"$a :& $n")}"
          }
          val fullRouteWithTracingMatcher = tracingFields
            .map(_ => p"$base ${Term.Name(s"usingFor${methodName.capitalize}")}(traceBuilder)")
            .getOrElse(base)
          val fullRouteWithTracingAndExtraction = customExtractionFields
            .map(_ => p"$fullRouteWithTracingMatcher ${Term.Name(s"extractorFor${methodName.capitalize}")}(extracted)")
            .getOrElse(fullRouteWithTracingMatcher)
          val fullRouteWithTracingAndExtractionAndAuth = authImplementation match {
            case Native => p"$fullRouteWithTracingAndExtraction ${Term.Name(s"as")}(authContext)"
            case _      => fullRouteWithTracingAndExtraction
          }

          fullRouteWithTracingAndExtractionAndAuth
        }
        val handlerCallArgs: List[List[Term]] = List(List(responseCompanionTerm)) ++ List(
          (authContext.map(_._1).toList ++ pathArgs ++ qsArgs ++ bodyArgs).map(_.paramName).toList ++ (http4sForm ++ http4sHeaders)
            .map(_.handlerCallArg)
        ) ++
          tracingFields.map(_.param.paramName).map(List(_)) ++
          customExtractionFields.map(_.param.paramName).map(List(_))
        val handlerCall = q"handler.${Term.Name(methodName)}(...${handlerCallArgs})"
        val isGeneric   = ResponseADTHelper.isDefinitionGeneric(responses)
        val responseExpr = ServerRawResponse(operation)
          .filter(_ == true)
          .fold[Term] {
            val marshallers = responses.value.map { case Response(statusCodeName, valueType, headers) =>
              val responseTerm  = Term.Name(s"${statusCodeName.value}")
              val baseRespType  = Type.Select(responseCompanionTerm, Type.Name(statusCodeName.value))
              val respType      = if (isGeneric) Type.Apply(baseRespType, List(t"F")) else baseRespType
              val generatorName = Term.Name(s"$methodName${statusCodeName}EntityResponseGenerator")
              val encoderName   = Term.Name(s"$methodName${statusCodeName}Encoder")
              (valueType, headers.value) match {
                case (None, Nil) =>
                  if (isGeneric) {
                    p"case $responseCompanionTerm.$responseTerm() => F.pure(Response[F](status = org.http4s.Status.${statusCodeName}))"
                  } else {
                    p"case $responseCompanionTerm.$responseTerm => F.pure(Response[F](status = org.http4s.Status.${statusCodeName}))"
                  }
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
            val (buildTerms, buildPat) =
              rest.foldLeft[(Term => Term, Pat => Pat)]((identity, identity)) { case ((accTerm, accPat), (nextTermGroup, nextPatGroup)) =>
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

        val includeAuthContext: Term => Term =
          authContext.fold[Term => Term](identity _)(_._2)

        val fullRoute: Case =
          authImplementation match {
            case Native =>
              p"""case authedReq @ $fullRouteMatcher =>
                  val req = authedReq.req
                  ${includeAuthContext(q"mapRoute($methodName, authedReq, { $routeBody })")}
                """
            case _ =>
              p"""case req @ $fullRouteMatcher =>
                  ${includeAuthContext(q"mapRoute($methodName, req, { $routeBody })")}
                """
          }

        val respond: List[List[Term.Param]] = List(List(param"respond: ${Term.Name(resourceName)}.$responseCompanionTerm.type"))

        val params: List[List[Term.Param]] = respond ++ orderedParameters.map(
          _.map(scalaParam =>
            scalaParam.param.copy(
              decltpe = if (scalaParam.isFile) {
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
            List.empty // handlerDefinitions
          )
        )
      })
  }

  private def renderCustomSecurityRequirements(sr: SecurityRequirements): Term = {
    val orElements = sr.requirements.toList.map { r =>
      val andElements = r.unwrapTracker.toSortedMap.toList.map { case (key, scopes) =>
        val renderedScopes = scopes.map(Lit.String(_))
        q"""(${Term.Name(authSchemesTypeName.value)}.${securitySchemeNameToClassName(key)} -> Set(..$renderedScopes))"""
      }
      q"""_root_.cats.data.NonEmptyMap.of(..$andElements)"""
    }
    q"""_root_.cats.data.NonEmptyList.of(..$orElements)"""
  }

  def createHttp4sHeaders(headers: List[Header[ScalaLanguage]]): (Term.Name, List[Defn.Val]) = {
    val (names, definitions) = headers.map { case Header(name, required, _, termName) =>
      val nameLiteral = Lit.String(name)
      val headerName  = Term.Name(s"${name.toCamelCase}Header")
      val pattern     = Pat.Var(headerName)
      val v           = if (required) q"List[Header.ToRaw](($nameLiteral, resp.$termName))" else q"resp.$termName.map[Header.ToRaw](($nameLiteral,_)).toList"
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
    } yield generatePathParamExtractors(pathArgs)

  def generatePathParamExtractors(pathArgs: List[LanguageParameter[ScalaLanguage]]): List[Defn] =
    pathArgs
      .map(_.argType)
      .flatMap {
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
      }
      .distinctBy(_._1)
      .map { case (name, tpe) =>
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
      }

  def modifiedOptionalMultiQueryParamDecoderMatcher(matcherName: Term.Name, container: Type, argName: Lit.String, tpe: Type, transform: Term => Term) =
    q"""
      object ${matcherName} {
        def unapply(params: Map[String, collection.Seq[String]]): Option[Option[$container[$tpe]]] = {
          val res = params.get(${argName}) match {
            case Some(values) =>
              Some(values.toList.traverse(s => QueryParamDecoder[${tpe}].decode(QueryParameterValue(s))))
            case None => Some(cats.data.Validated.Valid(Nil)) // absent
          }
          res.collectFirst { case cats.data.Validated.Valid(value) => ${transform(q"Option(value).filter(_.nonEmpty)")} }
        }
      }
    """

  def modifiedQueryParamDecoderMatcher(matcherName: Term.Name, container: Type, argName: Lit.String, tpe: Type, transform: Term => Term) =
    q"""
      object ${matcherName} {
        def unapply(params: Map[String, collection.Seq[String]]): Option[${container}[${tpe}]] = {
          ${transform(q"""params
            .get(${argName})
            .flatMap(values =>
              values.toList.traverse(s => QueryParamDecoder[${tpe}].decode(QueryParameterValue(s)).toOption))
          """)}
        }
      }
    """

  def generateQueryParamMatchers(methodName: String, qsArgs: List[LanguageParameter[ScalaLanguage]]): List[Defn] = {
    val (decoders, matchers) = qsArgs
      .traverse { case LanguageParameter(_, param, _, argName, argType) =>
        val containerTransformations = Map[String, Term => Term](
          "Iterable"   -> identity _,
          "List"       -> (term => q"$term.map(_.toList)"),
          "Vector"     -> (term => q"$term.map(_.toVector)"),
          "Seq"        -> (term => q"$term.map(_.toSeq)"),
          "IndexedSeq" -> (term => q"$term.map(_.toIndexedSeq)")
        )
        val matcherName = Term.Name(s"${methodName.capitalize}${argName.value.capitalize}Matcher")
        val (queryParamMatcher: Defn.Object, elemType: Type) = param match {
          case param"$_: Option[$container[$tpe]]" if containerTransformations.contains(container.syntax) =>
            val transform = containerTransformations(container.syntax)
            (modifiedOptionalMultiQueryParamDecoderMatcher(matcherName, container, argName.toLit, tpe, transform), tpe)
          case param"$_: Option[$container[$tpe]] = $_" if containerTransformations.contains(container.syntax) =>
            val transform = containerTransformations(container.syntax)
            (modifiedOptionalMultiQueryParamDecoderMatcher(matcherName, container, argName.toLit, tpe, transform), tpe)
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
            val transform = containerTransformations(container.syntax)
            (modifiedQueryParamDecoderMatcher(matcherName, container, argName.toLit, tpe, transform), tpe)
          case param"$_: $container[$tpe] = $_" if containerTransformations.contains(container.syntax) =>
            val transform = containerTransformations(container.syntax)
            (modifiedQueryParamDecoderMatcher(matcherName, container, argName.toLit, tpe, transform), tpe)
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
      }

    decoders.distinctBy(_._1.toString()).map(_._2) ++ matchers
  }

  /** It's not possible to use backticks inside pattern matching as it has different semantics: backticks inside match are just references to an already
    * existing bindings.
    */
  def prepareParameters[F[_]: Traverse](parameters: F[LanguageParameter[ScalaLanguage]]): Target[F[LanguageParameter[ScalaLanguage]]] =
    if (parameters.exists(param => param.paramName.syntax != param.paramName.value)) {
      // let's try to prefix them all with underscore and see if it helps
      for {
        _ <- Target.log.debug("Found that not all parameters could be represented as unescaped terms")
        res <- parameters.traverse[Target, LanguageParameter[ScalaLanguage]] { param =>
          for {
            _ <- Target.log.debug(s"Escaping param ${param.argName.value}")
            newName = Term.Name(s"_${param.paramName.value}")
            res <-
              if (newName.syntax == newName.value) Target.pure(param.withParamName(newName))
              else Target.raiseUserError(s"Can't escape parameter with name ${param.argName.value}.")
          } yield res
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
    bodyArgs.toList.flatMap { case LanguageParameter(_, _, _, _, argType) =>
      List(
        q"private[this] val ${Pat.Typed(Pat.Var(Term.Name(s"${methodName.uncapitalized}Decoder")), t"EntityDecoder[F, $argType]")} = ${ResponseADTHelper
            .generateDecoder(argType, consumes)}"
      )
    }

  def generateEncoders(methodName: String, responses: Responses[ScalaLanguage], produces: Seq[ContentType]): List[Defn.Val] =
    for {
      response    <- responses.value
      (_, tpe, _) <- response.value
    } yield {
      val contentTypes = response.value.map(_._1).map(List(_)).getOrElse(produces) // for OpenAPI 3.x we should take ContentType from the response
      q"private[this] val ${Pat.Var(Term.Name(s"$methodName${response.statusCodeName}Encoder"))} = ${ResponseADTHelper.generateEncoder(tpe, contentTypes)}"
    }

  def generateResponseGenerators(methodName: String, responses: Responses[ScalaLanguage]): List[Defn.Val] =
    for {
      response <- responses.value
      if response.value.nonEmpty
    } yield q"private[this] val ${Pat.Var(Term.Name(s"$methodName${response.statusCodeName}EntityResponseGenerator"))} = ${ResponseADTHelper
        .generateEntityResponseGenerator(q"org.http4s.Status.${response.statusCodeName}")}"

  def generateTracingExtractor(methodName: String, tracingField: Term): Defn.Object =
    q"""
       object ${Term.Name(s"usingFor${methodName.capitalize}")} {
         def unapply(r: Request[F]): Some[(Request[F], TraceBuilder[F])] = Some(r -> $tracingField(r))
       }
     """

  def generateCustomExtractionFieldsExtractor(methodName: String, extractField: Term): Defn.Object =
    q"""
       object ${Term.Name(s"extractorFor${methodName.capitalize}")} {
         def unapply(r: Request[F]): Some[(Request[F], $customExtractionTypeName)] = Some(r -> $extractField(r))
       }
     """
}
