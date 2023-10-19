package dev.guardrail.generators.scala.akkaHttp

import _root_.io.swagger.v3.oas.models.Components
import _root_.io.swagger.v3.oas.models.Operation
import _root_.io.swagger.v3.oas.models.PathItem.HttpMethod
import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import dev.guardrail._
import dev.guardrail.core.LiteralRawType
import dev.guardrail.core.MapRawType
import dev.guardrail.core.ReifiedRawType
import dev.guardrail.core.Tracker
import dev.guardrail.core.VectorRawType
import dev.guardrail.core.extract.ServerRawResponse
import dev.guardrail.core.extract.TracingLabel
import dev.guardrail.generators.CustomExtractionField
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.generators.RawParameterName
import dev.guardrail.generators.RenderedRoutes
import dev.guardrail.generators.Server
import dev.guardrail.generators.Servers
import dev.guardrail.generators.TracingField
import dev.guardrail.generators.operations.TracingLabelFormatter
import dev.guardrail.generators.scala.CirceModelGenerator
import dev.guardrail.generators.scala.CirceRefinedModelGenerator
import dev.guardrail.generators.scala.JacksonModelGenerator
import dev.guardrail.generators.scala.ModelGeneratorType
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.syntax._
import dev.guardrail.generators.spi.ModuleLoadResult
import dev.guardrail.generators.spi.ProtocolGeneratorLoader
import dev.guardrail.generators.spi.ServerGeneratorLoader
import dev.guardrail.generators.syntax._
import dev.guardrail.shims._
import dev.guardrail.terms.ApplicationJson
import dev.guardrail.terms.BinaryContent
import dev.guardrail.terms.CollectionsLibTerms
import dev.guardrail.terms.ContentType
import dev.guardrail.terms.Header
import dev.guardrail.terms.LanguageTerms
import dev.guardrail.terms.MultipartFormData
import dev.guardrail.terms.Response
import dev.guardrail.terms.Responses
import dev.guardrail.terms.RouteMeta
import dev.guardrail.terms.SecurityScheme
import dev.guardrail.terms.SwaggerTerms
import dev.guardrail.terms.TextContent
import dev.guardrail.terms.TextPlain
import dev.guardrail.terms.UrlencodedFormData
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.server._

import scala.meta._
import scala.reflect.runtime.universe.typeTag

class AkkaHttpServerGeneratorLoader extends ServerGeneratorLoader {
  type L = ScalaLanguage
  override def reified = typeTag[Target[ScalaLanguage]]
  val apply =
    ModuleLoadResult.forProduct2(
      ServerGeneratorLoader.label -> Seq(AkkaHttpVersion.mapping),
      ProtocolGeneratorLoader.label -> Seq(
        CirceModelGenerator.mapping,
        CirceRefinedModelGenerator.mapping.view.mapValues(_.toCirce).toMap,
        JacksonModelGenerator.mapping
      )
    ) { (akkaHttpVersion, collectionVersion) =>
      AkkaHttpServerGenerator(akkaHttpVersion, collectionVersion)
    }
}

object AkkaHttpServerGenerator {
  def apply(akkaHttpVersion: AkkaHttpVersion, modelGeneratorType: ModelGeneratorType): ServerTerms[ScalaLanguage, Target] =
    new AkkaHttpServerGenerator(akkaHttpVersion, modelGeneratorType)

  def generateUrlPathExtractors(
      path: Tracker[String],
      pathArgs: List[LanguageParameter[ScalaLanguage]],
      modelGeneratorType: ModelGeneratorType
  ): Target[NonEmptyList[(Term, List[Term.Name])]] =
    for {
      (parts, (trailingSlash, queryParams)) <- AkkaHttpPathExtractor.runParse(path, pathArgs, modelGeneratorType)
      allPairs <- parts
        .foldLeft[Target[NonEmptyList[(Term, List[Term.Name])]]](Target.pure(NonEmptyList.one((q"pathEnd", List.empty)))) { case (acc, (termName, b)) =>
          acc.flatMap {
            case NonEmptyList((q"pathEnd   ", bindings), xs) =>
              Target.pure(NonEmptyList((q"path(${b}       )", bindings ++ termName), xs))
            case NonEmptyList((q"path(${a})", bindings), xs) =>
              val newBindings = bindings ++ termName
              if (newBindings.length < 22) {
                Target.pure(NonEmptyList((q"path(${a} / ${b})", newBindings), xs))
              } else {
                Target.pure(NonEmptyList((q"pathEnd", List.empty), (q"pathPrefix(${a} / ${b})", newBindings) :: xs))
              }
            case nel =>
              Target.raiseUserError(s"Unexpected URL extractor state: ${nel}, (${termName}, ${b})")
          }
        }
      trailingSlashed <-
        if (trailingSlash) {
          allPairs match {
            case NonEmptyList((q"path(${a})", bindings), xs) => Target.pure(NonEmptyList((q"pathPrefix(${a}) & pathEndOrSingleSlash", bindings), xs))
            case NonEmptyList((q"pathEnd", bindings), xs)    => Target.pure(NonEmptyList((q"pathEndOrSingleSlash", bindings), xs))
            case nel                                         => Target.raiseUserError(s"Unexpected URL pattern state: ${nel}")
          }
        } else Target.pure(allPairs)
      result = queryParams.fold(trailingSlashed) { qs =>
        val NonEmptyList((directives, bindings), xs) = trailingSlashed
        NonEmptyList((q"${directives} & ${qs}", bindings), xs)
      }
    } yield result.reverse
}

class AkkaHttpServerGenerator private (akkaHttpVersion: AkkaHttpVersion, modelGeneratorType: ModelGeneratorType) extends ServerTerms[ScalaLanguage, Target] {
  val customExtractionTypeName: Type.Name = Type.Name("E")

  implicit def MonadF: Monad[Target] = Target.targetInstances

  override def fromSpec(context: Context, supportPackage: NonEmptyList[String], basePath: Option[String], frameworkImports: List[ScalaLanguage#Import])(
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[ScalaLanguage]],
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: SwaggerTerms[ScalaLanguage, Target]
  ): Target[Servers[ScalaLanguage]] = {
    import Sw._
    import Sc._

    for {
      extraImports       <- getExtraImports(context.tracing, supportPackage)
      supportDefinitions <- generateSupportDefinitions(context.tracing, securitySchemes)
      servers <- groupedRoutes.traverse { case (className, unsortedRoutes) =>
        val routes = unsortedRoutes
          .groupBy(_.path.unwrapTracker.indexOf('{'))
          .view
          .mapValues(_.sortBy(r => (r.path.unwrapTracker, r.method)))
          .toList
          .sortBy(_._1)
          .flatMap(_._2)
        for {
          resourceName <- formatTypeName(className.lastOption.getOrElse(""), Some("Resource"))
          handlerName  <- formatTypeName(className.lastOption.getOrElse(""), Some("Handler"))

          responseServerPair <- routes.traverse { case route @ RouteMeta(path, method, operation, securityRequirements) =>
            for {
              operationId           <- getOperationId(operation)
              responses             <- Responses.getResponses[ScalaLanguage, Target](operationId, operation, protocolElems, components)
              responseClsName       <- formatTypeName(operationId, Some("Response"))
              responseDefinitions   <- generateResponseDefinitions(responseClsName, responses, protocolElems)
              methodName            <- formatMethodName(operationId)
              parameters            <- route.getParameters[ScalaLanguage, Target](components, protocolElems)
              customExtractionField <- buildCustomExtractionFields(operation, className, context.customExtraction)
              tracingField          <- buildTracingFields(operation, className, context.tracing)
            } yield (
              responseDefinitions,
              GenerateRouteMeta(operationId, methodName, responseClsName, customExtractionField, tracingField, route, parameters, responses)
            )
          }
          (responseDefinitions, serverOperations) = responseServerPair.unzip
          securityExposure = serverOperations.flatMap(_.routeMeta.securityRequirements) match {
            case Nil => SecurityExposure.Undefined
            case xs  => if (xs.exists(_.optional)) SecurityExposure.Optional else SecurityExposure.Required
          }
          renderedRoutes <- generateRoutes(
            context.tracing,
            resourceName,
            handlerName,
            basePath,
            serverOperations,
            protocolElems,
            securitySchemes,
            securityExposure,
            context.authImplementation
          )
          handlerSrc <- renderHandler(
            handlerName,
            renderedRoutes.methodSigs,
            renderedRoutes.handlerDefinitions,
            responseDefinitions.flatten,
            context.customExtraction,
            context.authImplementation,
            securityExposure
          )
          extraRouteParams <- getExtraRouteParams(
            resourceName,
            context.customExtraction,
            context.tracing,
            context.authImplementation,
            securityExposure
          )
          classSrc <- renderClass(
            resourceName,
            handlerName,
            renderedRoutes.classAnnotations,
            renderedRoutes.routes,
            extraRouteParams,
            responseDefinitions.flatten,
            renderedRoutes.supportDefinitions,
            renderedRoutes.securitySchemesDefinitions,
            context.customExtraction,
            context.authImplementation
          )
        } yield Server[ScalaLanguage](className, frameworkImports ++ extraImports, handlerSrc, classSrc)
      }
    } yield Servers[ScalaLanguage](servers, supportDefinitions)
  }

  private def splitOperationParts(operationId: String): (List[String], String) = {
    val parts = operationId.split('.')
    (parts.drop(1).toList, parts.last)
  }

  private def generateResponseDefinitions(
      responseClsName: String,
      responses: Responses[ScalaLanguage],
      protocolElems: List[StrictProtocolElems[ScalaLanguage]]
  ) =
    for {
      _ <- Target.pure(())
      responseSuperType = Type.Name(responseClsName)
      responseSuperTerm = Term.Name(responseClsName)
      instances = responses.value.map { case resp @ Response(statusCodeName, _, headers) =>
        val statusCode       = q"StatusCodes.${statusCodeName}"
        val responseTerm     = Term.Name(s"${responseClsName}${statusCodeName.value}")
        val responseName     = Type.Name(s"${responseClsName}${statusCodeName.value}")
        val headerParams     = headers.value.map(h => (h.term, h.tpe))
        val headerParamTerms = headerParams.map { case (term, tpe) => param"${term}: ${tpe}" }
        val headersTerm      = generateHeaderParams(headers.value, Term.Name("r"))
        val headersExpr      = q"val allHeaders = $headersTerm"
        resp.value.fold[(Defn, Defn, Case)](
          if (headers.value.isEmpty) {
            (
              q"case object $responseTerm                      extends $responseSuperType($statusCode)",
              q"def $statusCodeName: $responseSuperType = $responseTerm",
              p"case r: $responseTerm.type => scala.concurrent.Future.successful(Marshalling.Opaque { () => HttpResponse(r.statusCode) } :: Nil)"
            )
          } else {
            (
              q"case class  $responseName(..${headerParams.map { case (term, tpe) => param"${term}: ${tpe}" }}) extends $responseSuperType($statusCode)",
              q"def $statusCodeName(..${headerParamTerms}): $responseSuperType = $responseTerm(..${headerParams.map { case (term, _) => q"${term}" }})",
              p"""case r :$responseName =>
                      $headersExpr;
                      scala.concurrent.Future.successful(Marshalling.Opaque { () => HttpResponse(r.statusCode, allHeaders) } :: Nil)"""
            )
          }
        ) { case (contentType, valueType, _) =>
          val transformer: Term => Term = contentType match {
            case _: TextPlain => x => q"TextPlain($x)"
            case _            => identity _
          }
          (
            q"case class  $responseName(..${param"value: ${valueType}" +: headerParamTerms}) extends $responseSuperType($statusCode)",
            q"def $statusCodeName(..${param"value: ${valueType}" +: headerParamTerms}): $responseSuperType = $responseTerm(..${q"value" +: headerParams
                .map { case (term, _) => q"${term}" }})",
            if (headers.value.isEmpty) {
              p"case r :$responseName => Marshal(${transformer(q"r.value")}).to[ResponseEntity].map { entity => Marshalling.Opaque { () => HttpResponse(r.statusCode, entity=entity) } :: Nil }"
            } else {
              p"""case r :$responseName =>
                 Marshal(${transformer(q"r.value")}).to[ResponseEntity].map { entity => Marshalling.Opaque { () =>
                 $headersExpr;
                 HttpResponse(r.statusCode, entity=entity, headers=allHeaders) } :: Nil }"""
            }
          )
        }
      }
      (terms, aliases, marshallers) = instances.unzip3
      convenienceConstructors = aliases.flatMap {
        case q"def $name(value: $tpe): $_ = $_" => tpe.map((_, name))
        case _                                  => None
      }
      implicitHelpers = convenienceConstructors
        .groupBy(_._1)
        .flatMap {
          case (tpe, (_, name) :: Nil) =>
            Some(tpe -> name)
          case _ =>
            None
        }
        .toList
        .sortBy(_._2.value)
        .map { case (tpe, name) =>
          q"implicit def ${Term.Name(s"${name.value}Ev")}(value: ${tpe}): ${responseSuperType} = ${name}(value)"
        }
      protocolImplicits <- AkkaHttpHelper.protocolImplicits(modelGeneratorType)
      toResponseImplicits = List(param"ec: scala.concurrent.ExecutionContext") ++ protocolImplicits
      companion = q"""
          object ${responseSuperTerm} {
          ${Defn.Def(
          List(mod"implicit"),
          Term.Name(s"${responseClsName.uncapitalized}TRM"),
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(Nil),
              NonEmptyList.fromList(protocolImplicits).fold(List.empty[Term.ParamClause])(nel => List(Term.ParamClause(nel.toList, Some(Mod.Implicit()))))
            )
          ),
          Some(t"ToResponseMarshaller[${responseSuperType}]"),
          q"""Marshaller { implicit ec => resp => ${Term.Name(s"${responseClsName.uncapitalized}TR")}(resp) }"""
        )};
            implicit def ${Term
          .Name(
            s"${responseClsName.uncapitalized}TR"
          )}(value: ${responseSuperType})(..${Term.ParamClause(
          toResponseImplicits,
          Some(Mod.Implicit())
        )}): scala.concurrent.Future[List[Marshalling[HttpResponse]]] =
              ${Term.Match(Term.Name("value"), marshallers, Nil)}

            def apply[T](value: T)(implicit ev: T => ${responseSuperType}): ${responseSuperType} = ev(value)

            ..${implicitHelpers}

            ..${aliases}
          }
        """
    } yield List[Defn](
      q"sealed abstract class ${responseSuperType}(val statusCode: StatusCode)"
    ) ++ terms ++ List[Defn](
      companion
    )

  private def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean) =
    for {
      _ <- Target.log.debug(s"buildCustomExtractionFields(${operation.unwrapTracker.showNotNull}, ${resourceName}, ${customExtraction})")
      res <-
        if (customExtraction) {
          for {
            operationId <- operation
              .downField("operationId", _.getOperationId())
              .map(_.map(splitOperationParts(_)._2))
              .raiseErrorIfEmpty("Missing operationId")
            operationIdTarget <- Target.pure(Lit.String(operationId.unwrapTracker))
          } yield Some(
            CustomExtractionField[ScalaLanguage](
              LanguageParameter.fromParam(Term.Param(List(), Term.Name("extracted"), Some(customExtractionTypeName), None)),
              q"""customExtract(${operationIdTarget})"""
            )
          )
        } else Target.pure(None)
    } yield res

  private def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean) =
    for {
      _ <- Target.log.debug(s"buildTracingFields(${operation.unwrapTracker.showNotNull}, ${resourceName}, ${tracing})")
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
          } yield Some(TracingField[ScalaLanguage](LanguageParameter.fromParam(param"traceBuilder: TraceBuilder"), q"""trace(${label})"""))
        } else Target.pure(None)
    } yield res
  private def generateRoutes(
      tracing: Boolean,
      resourceName: String,
      handlerName: String,
      basePath: Option[String],
      routes: List[GenerateRouteMeta[ScalaLanguage]],
      protocolElems: List[StrictProtocolElems[ScalaLanguage]],
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
      securityExposure: SecurityExposure,
      authImplementation: AuthImplementation
  ) =
    for {
      renderedRoutes <- routes.traverse(generateRoute(resourceName, basePath))
      routeTerms = renderedRoutes.map(_.route)
      combinedRouteTerms <- combineRouteTerms(routeTerms)
      methodSigs = renderedRoutes.map(_.methodSig)
    } yield RenderedRoutes[ScalaLanguage](
      List(combinedRouteTerms),
      List.empty,
      methodSigs,
      renderedRoutes.flatMap(_.supportDefinitions),
      renderedRoutes.flatMap(_.handlerDefinitions),
      List.empty
    )
  private def renderHandler(
      handlerName: String,
      methodSigs: List[scala.meta.Decl.Def],
      handlerDefinitions: List[scala.meta.Stat],
      responseDefinitions: List[scala.meta.Defn],
      customExtraction: Boolean,
      authImplementation: AuthImplementation,
      securityExposure: SecurityExposure
  ) =
    for {
      _ <- Target.log.debug(s"renderHandler(${handlerName}, ${methodSigs}")
    } yield {
      val tParams = if (customExtraction) List(tparam"-$customExtractionTypeName") else List()
      q"""
        trait ${Type.Name(handlerName)}[..${tParams}] {
          ..${methodSigs ++ handlerDefinitions}
        }
      """
    }
  private def getExtraRouteParams(
      resourceName: String,
      customExtraction: Boolean,
      tracing: Boolean,
      authImplementation: AuthImplementation,
      securityExposure: SecurityExposure
  ) =
    for {
      _ <- Target.log.debug(s"getExtraRouteParams(${tracing})")
      extractParam <-
        if (customExtraction) {
          Target.pure(List(param"""customExtract: String => Directive1[$customExtractionTypeName]"""))
        } else Target.pure(List.empty)
      traceParam <-
        if (tracing) {
          Target.pure(List(param"""trace: String => Directive1[TraceBuilder]"""))
        } else Target.pure(List.empty)
    } yield extractParam ::: traceParam
  private def renderClass(
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
    for {
      _                 <- Target.log.debug(s"renderClass(${resourceName}, ${handlerName}, <combinedRouteTerms>, ${extraRouteParams})")
      protocolImplicits <- AkkaHttpHelper.protocolImplicits(modelGeneratorType)
    } yield {
      val handlerType = {
        val baseHandlerType = Type.Name(handlerName)
        if (customExtraction) {
          t"${baseHandlerType}[${customExtractionTypeName}]"
        } else {
          baseHandlerType
        }
      }
      val typeParams = if (customExtraction) List(tparam"$customExtractionTypeName") else List()
      val params = List(
        Term.ParamClause(
          List(param"handler: $handlerType") ++ extraRouteParams,
          None
        ),
        Term.ParamClause(List(param"mat: akka.stream.Materializer") ++ protocolImplicits, Some(Mod.Implicit()))
      )
      List(q"""
        object ${Term.Name(resourceName)} {
          ..${supportDefinitions};
          def routes[..${typeParams}](...${params}): Route = {
            ..${combinedRouteTerms}
          }

          ..${responseDefinitions}
        }
      """)
    }
  private def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]) =
    for {
      _ <- Target.log.debug(s"getExtraImports(${tracing})")
    } yield List(
      if (tracing) Option(q"import akka.http.scaladsl.server.Directive1") else None,
      Option(q"import scala.language.higherKinds")
    ).flatten

  private def generateSupportDefinitions(
      tracing: Boolean,
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
  ) =
    Target.pure(List.empty)

  private def httpMethodToAkka(method: HttpMethod): Target[Term] = method match {
    case HttpMethod.DELETE  => Target.pure(q"delete")
    case HttpMethod.GET     => Target.pure(q"get")
    case HttpMethod.PATCH   => Target.pure(q"patch")
    case HttpMethod.POST    => Target.pure(q"post")
    case HttpMethod.PUT     => Target.pure(q"put")
    case HttpMethod.OPTIONS => Target.pure(q"options")
    case HttpMethod.HEAD    => Target.pure(q"head")
    case other              => Target.raiseUserError(s"Unknown method: ${other}")
  }

  private def pathStrToAkka(
      basePath: Option[String],
      path: Tracker[String],
      pathArgs: List[LanguageParameter[ScalaLanguage]]
  ): Target[NonEmptyList[(Term, List[Term.Name])]] = {

    def addTrailingSlashMatcher(trailingSlash: Boolean, term: Term.Apply): Term =
      if (trailingSlash)
        q"${term.copy(fun = Term.Name("pathPrefix"))} & pathEndOrSingleSlash"
      else term

    (basePath.getOrElse("") + path.unwrapTracker).stripPrefix("/") match {
      case "" => Target.pure(NonEmptyList.one((q"pathEndOrSingleSlash", List.empty)))
      case fullPath =>
        AkkaHttpServerGenerator.generateUrlPathExtractors(Tracker.cloneHistory(path, fullPath), pathArgs, modelGeneratorType)
    }
  }

  private def findInnerTpe(rawType: ReifiedRawType): LiteralRawType = rawType match {
    case x: LiteralRawType    => x
    case VectorRawType(inner) => findInnerTpe(inner)
    case MapRawType(inner)    => findInnerTpe(inner)
  }

  private def directivesFromParams(
      required: Term => Type => Option[Term] => Target[Term],
      multi: Term => Type => Option[Term] => (Term => Term) => Target[Term],
      multiOpt: Term => Type => Option[Term] => (Term => Term) => Target[Term],
      optional: Term => Type => Option[Term] => Target[Term]
  )(params: List[LanguageParameter[ScalaLanguage]]): Target[Option[(Term, List[Term.Name])]] =
    for {
      directives <- params.traverse[Target, Term] { case sparam @ LanguageParameter(_, param, _, argName, argType) =>
        val containerTransformations = Map[String, Term => Term](
          "Iterable"   -> identity _,
          "List"       -> (term => q"$term.toList"),
          "Vector"     -> (term => q"$term.toVector"),
          "Seq"        -> (term => q"$term.toSeq"),
          "IndexedSeq" -> (term => q"$term.toIndexedSeq")
        )

        val unmarshaller: Type => Option[Term] = tpe =>
          findInnerTpe(sparam.rawType) match {
            case LiteralRawType(Some("string"), _) => Some(q"stringyJsonUnmarshaller.andThen(unmarshallJson[${tpe}])")
            case _                                 => Option.empty
          }
        param match {
          case param"$_: Option[$container[$tpe]]" if containerTransformations.contains(container.syntax) =>
            multiOpt(argName.toLit)(tpe)(unmarshaller(tpe))(containerTransformations(container.syntax))
          case param"$_: Option[$container[$tpe]] = $_" if containerTransformations.contains(container.syntax) =>
            multiOpt(argName.toLit)(tpe)(unmarshaller(tpe))(containerTransformations(container.syntax))
          case param"$_: Option[$tpe]" =>
            optional(argName.toLit)(tpe)(unmarshaller(tpe))
          case param"$_: Option[$tpe] = $_" =>
            optional(argName.toLit)(tpe)(unmarshaller(tpe))

          case param"$_: $container[$tpe]" =>
            multi(argName.toLit)(tpe)(unmarshaller(tpe))(containerTransformations(container.syntax))
          case param"$_: $container[$tpe] = $_" =>
            multi(argName.toLit)(tpe)(unmarshaller(tpe))(containerTransformations(container.syntax))

          case param"$_: $tpe = $_" =>
            required(argName.toLit)(argType)(tpe.flatMap(unmarshaller))
          case param"$_: $tpe" =>
            required(argName.toLit)(argType)(tpe.flatMap(unmarshaller))
          case other =>
            Target.raiseException(s"Unexpected parameter format when generating directives: ${other}")
        }
      }
    } yield directives match {
      case Nil => Option.empty
      case x :: xs =>
        Some((xs.foldLeft[Term](x) { case (a, n) => q"${a} & ${n}" }, params.map(_.paramName)))
    }

  private def bodyToAkka(methodName: String, body: Option[LanguageParameter[ScalaLanguage]]): Target[Option[Term]] =
    Target.pure(
      body.map { case LanguageParameter(_, _, _, _, argType) =>
        q"entity(as[${argType}](${Term.Name(s"${methodName}Decoder")}))"
      }
    )

  private def headersToAkka: List[LanguageParameter[ScalaLanguage]] => Target[Option[(Term, List[Term.Name])]] =
    directivesFromParams(
      arg => {
        case t"String" =>
          _ => Target.pure(q"headerValueByName(${arg})")
        case tpe =>
          um =>
            Target.pure(
              q"""
              headerValueByName(${arg})
                .flatMap(str =>
                  onComplete(${um
                  .fold[Term => Term](identity)(um => term => q"${term}(${um}, mat.executionContext, mat)")
                  .apply(q"Unmarshal(str).to[${tpe}]")})
                    .flatMap[Tuple1[${tpe}]]({
                      case Failure(e) => reject(MalformedHeaderRejection(${arg}, e.getMessage, Some(e)))
                      case Success(x) => provide(x)
                    }))
            """
            )
      },
      arg => tpe => _ => _ => Target.raiseUserError(s"Unsupported Iterable[${arg}]"),
      arg => tpe => _ => _ => Target.raiseUserError(s"Unsupported Option[Iterable[${arg}]]"),
      arg => {
        case t"String" =>
          _ => Target.pure(q"optionalHeaderValueByName(${arg})")
        case tpe =>
          um =>
            Target.pure(
              q"""
              optionalHeaderValueByName(${arg})
                .flatMap(
                  _.fold[Directive1[Option[${tpe}]]](provide(Option.empty[${tpe}]))(str =>
                    onComplete(${um
                  .fold[Term => Term](identity)(um => term => q"${term}(${um}, mat.executionContext, mat)")
                  .apply(q"Unmarshal(str).to[${tpe}]")})
                      .flatMap[Tuple1[Option[${tpe}]]]({
                        case Failure(e) => reject(MalformedHeaderRejection(${arg}, e.getMessage, Some(e)))
                        case Success(x) => provide(Option(x))
                      })))
            """
            )
      }
    ) _

  private def qsToAkka: List[LanguageParameter[ScalaLanguage]] => Target[Option[(Term, List[Term.Name])]] = {
    type Unmarshaller = Term
    type Arg          = Term
    val nameReceptacle: Arg => Type => Term = arg => tpe => q"Symbol(${arg}).as[${tpe}]"
    val param: Option[Unmarshaller] => Arg => Type => Term =
      _.fold[Arg => Type => Term](nameReceptacle)(um => nameReceptacle.map(_.map(term => q"${term}(${um})")))
    directivesFromParams(
      arg => tpe => um => Target.pure(q"parameter(${param(um)(arg)(tpe)})"),
      arg => tpe => um => tf => Target.pure(q"parameter(${param(um)(arg)(tpe)}.*).map(x => ${tf(q"x")})"),
      arg => tpe => um => tf => Target.pure(q"parameter(${param(um)(arg)(tpe)}.*).map(xs => Option(xs).filterNot(_.isEmpty).map(x => ${tf(q"x")}))"),
      arg => tpe => um => Target.pure(q"parameter(${param(um)(arg)(tpe)}.?)")
    ) _
  }

  class Binding(val value: String) {
    def toPat: Pat        = if (value.nonEmpty && ('A'.to('Z').contains(value(0)))) toTerm else toVar
    def toVar: Pat.Var    = Pat.Var(toTerm)
    def toTerm: Term.Name = Term.Name(value)
    def toType: Type.Name = Type.Name(value)

    override def toString(): String = s"Binding($value)"
  }

  private def formToAkka(consumes: Tracker[NonEmptyList[ContentType]], methodName: String)(
      params: List[LanguageParameter[ScalaLanguage]]
  ): Target[(Option[Term], List[Stat])] = Target.log.function("formToAkka") {
    for {
      _ <-
        if (params.exists(_.isFile) && !consumes.exists(ct => ContentType.isSubtypeOf[MultipartFormData](ct.unwrapTracker))) {
          Target.log.warning(s"type: file detected, automatically enabling multipart/form-data handling (${consumes.showHistory})")
        } else {
          Target.pure(())
        }

      hasFile              = params.exists(_.isFile)
      urlencoded           = consumes.exists(ct => ContentType.isSubtypeOf[UrlencodedFormData](ct.unwrapTracker))
      multipart            = consumes.exists(ct => ContentType.isSubtypeOf[MultipartFormData](ct.unwrapTracker))
      referenceAccumulator = q"fileReferences"

      result <- NonEmptyList
        .fromList(params)
        .traverse { params =>
          val partsTerm = Term.Name(s"${methodName}Parts")
          val (multipartContainers, unmarshallers, matchers, termPatterns, optionalTermPatterns, unpacks, termTypes, grabHeads) = params.map {
            case rawParameter @ LanguageParameter(a, param, paramName, argName, argType) =>
              val containerName    = new Binding(paramName.value)
              val unmarshallerName = new Binding(s"Unmarshal${paramName.value}Part")
              val binding          = new Binding(paramName.value)
              val collected        = new Binding(s"${paramName.value}O")

              val isFile: Boolean = rawParameter.isFile
              val (isOptional, realType): (Boolean, Type) = argType match {
                case t"Option[$x]" => (true, x)
                case x             => (!rawParameter.required, x)
              }
              def interpolateUnmarshaller(unmarshaller: Term, liftUnmarshaller: Term => Term, patTerms: (Pat, Term)): (Defn.Val, Case, Defn.Val) =
                (
                  q"""
                        val ${unmarshallerName.toVar}: Unmarshaller[Multipart.FormData.BodyPart, ${Type
                      .Select(partsTerm, containerName.toType)}] = ${unmarshaller}
                      """,
                  Case(
                    argName.toLit,
                    None,
                    q"""
                          SafeUnmarshaller(${liftUnmarshaller(unmarshallerName.toTerm)}).apply(part)
                      """
                  ), {
                    val (pats, terms) = patTerms
                    q"""
                        val ${collected.toVar} = successes.collectFirst(${Term.PartialFunction(
                        List(
                          Case(
                            Pat.Extract(Term.Select(partsTerm, containerName.toTerm), Pat.ArgClause(List(pats))),
                            None,
                            terms
                          )
                        )
                      )})
                      """
                  }
                )

              val (unmarshaller, caseMatch, grabHead) = isFile match {
                case true =>
                  val (targetFunctor, targetHashName) = rawParameter.hashAlgorithm.fold[(Type, Term)]((t"Option", q"None"))(x => (t"Id", Lit.String(x)))
                  interpolateUnmarshaller(
                    q"""
                        (
                          handler.${Term.Name(s"${methodName}UnmarshalToFile")}[${targetFunctor}](${targetHashName}, handler.${Term
                        .Name(s"${methodName}MapFileField")}(_, _, _))
                            .map({ case (v1, v2, v3, v4) =>
                              ${Term.Select(partsTerm, containerName.toTerm)}((..${List(q"v1", q"v2", q"v3") ++ rawParameter.hashAlgorithm
                        .map(Function.const(q"v4"))}))
                            })
                        )
                      """,
                    wrapped => q"AccumulatingUnmarshaller(${referenceAccumulator}, ${wrapped})(_.value._1)",
                    (
                      p"(..${List(p"v1", p"v2", p"v3") ++ rawParameter.hashAlgorithm.map(Function.const(p"v4"))})",
                      q"(..${List(q"v1", q"v2", q"v3") ++ rawParameter.hashAlgorithm.map(Function.const(q"v4"))})"
                    )
                  )
                case false =>
                  val textPlainUnmarshaller = findInnerTpe(rawParameter.rawType) match {
                    case LiteralRawType(Some("string"), _) => q"MFDBPviaFSU(stringyJsonEntityUnmarshaller.andThen(unmarshallJson[${realType}]))"
                    case _                                 => q"MFDBPviaFSU(sneakyJsonEntityUnmarshaller.andThen(unmarshallJson[${realType}]))"
                  }
                  val jsonUnmarshaller = q"MFDBPviaFSU(structuredJsonEntityUnmarshaller.andThen(unmarshallJson[${realType}]))"
                  val unmarshaller     = q"Unmarshaller.firstOf(${textPlainUnmarshaller}, ${jsonUnmarshaller})"
                  interpolateUnmarshaller(
                    q"""
                        Unmarshaller { implicit executionContext => part =>
                          ${unmarshaller}
                            .apply(part)
                            .map(${Term.Select(partsTerm, containerName.toTerm)}.apply)
                            .recoverWith({
                              case ex =>
                                Future.failed(RejectionError(MalformedFormFieldRejection(part.name, ex.getMessage, Some(ex))))
                            })
                        }
                      """,
                    identity,
                    (p"v1", q"v1")
                  )
              }

              val partContainer = q"case class ${containerName.toType}(..${List(param"value: ${realType}")}) extends Part"

              val unpacker = if (isOptional) {
                enumerator""" ${binding.toVar} <- Either.right[MissingFormFieldRejection, ${argType}](${collected.toTerm}) """
              } else {
                enumerator""" ${binding.toVar} <- ${collected.toTerm}.toRight(MissingFormFieldRejection(${argName.toLit})) """
              }

              (partContainer, unmarshaller, caseMatch, binding, collected, unpacker, argType, grabHead)
          }.unzip8

          val optionalTuple = optionalTermPatterns match {
            case binding :: Nil => p"Tuple1(${binding.toPat})"
            case xs             => p"(..${xs.map(_.toPat)})"
          }

          val _trait      = q"sealed trait Part"
          val ignoredPart = q"case class IgnoredPart(unit: Unit) extends Part"
          val ignoredUnmarshaller = p"""
        case _ =>
          SafeUnmarshaller(
          implicitly[Unmarshaller[Multipart.FormData.BodyPart, Unit]]
                                                      .map(${partsTerm}.IgnoredPart.apply(_))
        ).apply(part)
      """

          val fieldNames = q"""Set[String](..${params.toList.map(_.argName.toLit)})"""
          val optionalTypes = termTypes.map {
            case x @ t"Option[$_]" => x
            case x                 => t"Option[$x]"
          } match {
            case tpe :: Nil => t"Tuple1[${tpe}]"
            case xs         => t"(..${xs})"
          }
          val unpackedTypes = termTypes match {
            case tpe :: Nil => t"Tuple1[${tpe}]"
            case xs         => t"(..${xs})"
          }

          val allCases: List[Case] = matchers ++ List(ignoredUnmarshaller)

          type EntityDirective = Term
          val (handlerDefinitions, trackFileStuff): (List[Stat], EntityDirective => Term) = if (hasFile) {
            (
              List(
                q"""
          def ${Term.Name(s"${methodName}MapFileField")}(fieldName: String, fileName: Option[String], contentType: ContentType): File
          """,
                q"""
            def ${Term
                    .Name(
                      s"${methodName}UnmarshalToFile"
                    )}[F[_]: Functor](hashType: F[String], destFn: (String, Option[String], ContentType) => File)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, (File, Option[String], ContentType, F[String])] = Unmarshaller { implicit executionContext => part =>
              val dest = destFn(part.name, part.filename, part.entity.contentType)
              val messageDigest = hashType.map(MessageDigest.getInstance(_))
              val fileSink: Sink[ByteString,Future[IOResult]] = FileIO.toPath(dest.toPath).contramap[ByteString] { chunk => val _ = messageDigest.map(_.update(chunk.toArray[Byte])); chunk }

              part.entity.dataBytes.toMat(fileSink)(Keep.right).run()
                .transform(${Term.PartialFunction(
                    List(
                      if (akkaHttpVersion == AkkaHttpVersion.V10_1)
                        Some(p"""
                    case IOResult(_, Failure(t)) =>
                      dest.delete()
                      throw t
                  """)
                      else None,
                      Some(p"""
                  case IOResult(_, _) =>
                    val hash = messageDigest.map(md => javax.xml.bind.DatatypeConverter.printHexBinary(md.digest()).toLowerCase(java.util.Locale.US))
                    (dest, part.filename, part.entity.contentType, hash)
                  """)
                    ).flatten
                  )}, { case t =>
                  dest.delete()
                  t
                })
            }
          """
              ),
              entityDirective =>
                q"""
        (
          handleExceptions(ExceptionHandler {
            case e: Throwable =>
              ${referenceAccumulator}.get().foreach(_.delete())
              throw e
          }) & extractSettings.flatMap({ settings =>
            handleRejections({ rejections: scala.collection.immutable.Seq[Rejection] =>
              ${referenceAccumulator}.get().foreach(_.delete())
              rejections.collectFirst {
                case MalformedRequestContentRejection(msg, EntityStreamSizeException(limit, contentLength)) =>
                  val summary = contentLength match {
                    case Some(cl) => s"Request Content-Length of $$cl bytes exceeds the configured limit of $$limit bytes"
                    case None     => s"Aggregated data length of request entity exceeds the configured limit of $$limit bytes"
                  }
                  val info = new ErrorInfo(summary, "Consider increasing the value of akka.http.server.parsing.max-content-length")
                  val status = ${akkaHttpVersion match {
                    case AkkaHttpVersion.V10_1 => q"StatusCodes.RequestEntityTooLarge"
                    case AkkaHttpVersion.V10_2 => q"StatusCodes.PayloadTooLarge"
                  }}
                  val msg = if (settings.verboseErrorMessages) info.formatPretty else info.summary
                  complete(HttpResponse(status, entity = msg))
              }
            })
          }) & mapResponse({ resp =>
            ${referenceAccumulator}.get().foreach(_.delete())
            resp
          }) & ${entityDirective}
        )
        """
            )
          } else (List.empty[Stat], term => term)

          for {
            (handlers, unmarshallerTerms) <- consumes
              .map(_.distinct)
              .indexedDistribute
              .traverse[Target, (List[Stat], NonEmptyList[Term.Name])] {
                case Tracker(_, _: MultipartFormData) =>
                  val unmarshallerTerm = q"MultipartFormDataUnmarshaller"
                  val fru = q"""
                object ${partsTerm} {
                  ..${List(
                      _trait,
                      ignoredPart
                    ) ++ multipartContainers}
                }

                ..${unmarshallers};

                val ${Pat.Var(referenceAccumulator)} = new AtomicReference(List.empty[File])
                implicit val ${Pat.Var(unmarshallerTerm)}: FromRequestUnmarshaller[Either[Throwable, ${optionalTypes}]] =
                  implicitly[FromRequestUnmarshaller[Multipart.FormData]].flatMap { implicit executionContext => implicit mat => formData =>
                    val collectedPartsF: Future[Either[Throwable, ${optionalTypes}]] = for {
                      results <- formData.parts
                        .mapConcat({ part =>
                          if (${fieldNames}.contains(part.name)) part :: Nil
                          else {
                            part.entity.discardBytes()
                            Nil
                          }
                        }).mapAsync(1)(${Term.Block(
                      List(Term.Function(Term.ParamClause(List(Term.Param(List.empty, q"part", None, None))), Term.Match(q"part.name", allCases, Nil)))
                    )})
                          .toMat(Sink.seq[Either[Throwable, ${Type.Select(partsTerm, Type.Name("Part"))}]])(Keep.right).run()
                      } yield {
                        results.toList.sequence.map({ successes =>
                          ..${grabHeads}

                          ${optionalTermPatterns.map(_.toTerm) match {
                      case term :: Nil => q"Tuple1(${term})"
                      case xs          => q"(..${xs})"
                    }}
                        })
                      }

                    collectedPartsF
                  }
                """.stats
                  Target.pure((fru, NonEmptyList.one(unmarshallerTerm)))

                case Tracker(_, _: UrlencodedFormData) =>
                  for {
                    unmarshalFieldTypeParam        <- AkkaHttpHelper.unmarshalFieldTypeParam(modelGeneratorType)
                    unmarshalFieldUnmarshallerType <- AkkaHttpHelper.unmarshalFieldUnmarshallerType(modelGeneratorType)
                    unmarshallerTerm = q"FormDataUnmarshaller"
                    fru = q"""
                implicit val ${Pat.Var(unmarshallerTerm)}: FromRequestUnmarshaller[Either[Throwable, ${optionalTypes}]] =
                  implicitly[FromRequestUnmarshaller[FormData]].flatMap { implicit executionContext => implicit mat => formData =>
                    def unmarshalField[${unmarshalFieldTypeParam}](name: String, value: String, unmarshaller: Unmarshaller[String, ${unmarshalFieldUnmarshallerType}]): Future[A] =
                      unmarshaller.andThen(jsonDecoderUnmarshaller[A]).apply(value).recoverWith({
                        case ex =>
                          Future.failed(RejectionError(MalformedFormFieldRejection(name, ex.getMessage, Some(ex))))
                      })

                    ${params
                        .map(param =>
                          if (param.isFile) {
                            q"Future.successful(Option.empty[(File, Option[String], ContentType)])"
                          } else {
                            val (realType, getFunc, transformResponse): (Type, Term.Name, Term => Term) = param.argType match {
                              case t"Iterable[$x]"         => (x, q"getAll", (x: Term) => q"${x}.map(Option.apply)")
                              case t"Option[Iterable[$x]]" => (x, q"getAll", (x: Term) => q"${x}.map(Option.apply)")
                              case t"List[$x]"             => (x, q"getAll", (x: Term) => q"${x}.map(Option.apply)")
                              case t"Option[List[$x]]"     => (x, q"getAll", (x: Term) => q"${x}.map(Option.apply)")
                              case t"Vector[$x]"           => (x, q"getAll", (x: Term) => q"${x}.map(xs => Option(xs.toVector))")
                              case t"Option[Vector[$x]]"   => (x, q"getAll", (x: Term) => q"${x}.map(xs => Option(xs.toVector))")
                              case t"Option[$x]"           => (x, q"get", (x: Term) => x)
                              case x                       => (x, q"get", (x: Term) => x)
                            }
                            val unmarshaller = findInnerTpe(param.rawType) match {
                              case LiteralRawType(Some("string"), _) => q"jsonStringyUnmarshaller"
                              case _                                 => q"jsonParsingUnmarshaller"
                            }
                            transformResponse(
                              q"""formData.fields.${getFunc}(${param.argName.toLit}).traverse(unmarshalField[${realType}](${param.argName.toLit}, _, ${unmarshaller}))"""
                            )
                          }
                        ) match {
                        case NonEmptyList(term, Nil)   => q"${term}.map(v1 => Right(Tuple1(v1)))"
                        case NonEmptyList(term, terms) => q"(..${term +: terms}).mapN(${Term.Name(s"Tuple${terms.length + 1}")}.apply).map(Right.apply)"
                      }}
                  }
              """
                  } yield (List(fru), NonEmptyList.one(unmarshallerTerm))

                case Tracker(hist, _: ApplicationJson) =>
                  Target.raiseUserError(s"Unable to generate unmarshaller for application/json (${hist})")

                case Tracker(hist, ct: BinaryContent) => Target.raiseUserError(s"Unable to generate unmarshaller for ${ct.value} (${hist})")

                case Tracker(hist, ct: TextContent) => Target.raiseUserError(s"Unable to generate unmarshaller for ${ct.value} (${hist})")

                case Tracker(hist, ct) => Target.raiseException(s"Unexpected ContentType ${ct} (${hist})")
              }
              .map(_.flatSequence)

            directive =
              q"""
        ({
          ..${handlers}
          (
            ${trackFileStuff(q"entity(as(Unmarshaller.firstOf(..${unmarshallerTerms.toList})))")}
          ).flatMap(_.fold({
            case RejectionError(rej) => reject(rej)
            case t => throw t
          }, ${Term.PartialFunction(
                  List(
                    Case(
                      optionalTuple,
                      None,
                      q"""
            val maybe: Either[Rejection, ${unpackedTypes}] = for {
              ..${unpacks}
            } yield {
              ${termPatterns.map(_.toTerm) match {
                          case term :: Nil => q"Tuple1(${term})"
                          case xs          => q"(..${xs})"
                        }}
            }
            maybe.fold(reject(_), tprovide(_))
          """
                    )
                  )
                )}))
        }: Directive[${unpackedTypes}])
      """
          } yield (Option(directive), handlerDefinitions)

        }
    } yield result.getOrElse((Option.empty[Term], List.empty[Stat]))
  }

  private case class RenderedRoute(route: Term, methodSig: Decl.Def, supportDefinitions: List[Defn], handlerDefinitions: List[Stat])

  private def generateRoute(
      resourceName: String,
      basePath: Option[String]
  ): GenerateRouteMeta[ScalaLanguage] => Target[RenderedRoute] = {
    case GenerateRouteMeta(_, methodName, responseClsName, customExtractionFields, tracingFields, route, parameters, responses) =>
      // Generate the pair of the Handler method and the actual call to `complete(...)`
      for {
        _ <- Target.log.debug(s"generateRoute(${resourceName}, ${basePath}, ${route}, ${customExtractionFields}, ${tracingFields})")
        RouteMeta(path, method, operation, securityRequirements) = route
        consumes = operation
          .downField("consumes", _.consumes)
          .map(xs =>
            NonEmptyList
              .fromList(xs.flatMap(ContentType.unapply(_)))
              .getOrElse(NonEmptyList.one(ApplicationJson.empty))
          )

        // special-case file upload stuff
        formArgs = parameters.formParams.map { x =>
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
        }
        headerArgs = parameters.headerParams
        pathArgs   = parameters.pathParams
        qsArgs     = parameters.queryStringParams
        bodyArgs   = parameters.bodyParams

        akkaPath                       <- pathStrToAkka(basePath, path, pathArgs)
        akkaMethod                     <- httpMethodToAkka(method)
        akkaQs                         <- qsArgs.grouped(22).toList.flatTraverse(args => qsToAkka(args).map(_.toList))
        akkaBody                       <- bodyToAkka(methodName, bodyArgs)
        (akkaForm, handlerDefinitions) <- formToAkka(consumes, methodName)(formArgs)
        akkaHeaders                    <- headerArgs.grouped(22).toList.flatTraverse(args => headersToAkka(args).map(_.toList))
        // We aren't capitalizing the response names in order to keep back compat
        (responseCompanionTerm, responseCompanionType) = (Term.Name(responseClsName), Type.Name(responseClsName))
        responseType =
          if (ServerRawResponse(operation).getOrElse(false)) {
            t"HttpResponse"
          } else {
            t"${Term.Name(resourceName)}.${responseCompanionType}"
          }
        extractionTracingParameters = (tracingFields.map(_.param).toList ::: customExtractionFields.map(_.param).toList) match {
          case Nil => None
          case l   => Some(l)
        }
        orderedParameters = List((pathArgs ++ qsArgs ++ bodyArgs ++ formArgs ++ headerArgs).toList) ++ extractionTracingParameters

        entityProcessor = akkaBody.orElse(akkaForm).getOrElse(q"discardEntity")
        fullRouteMatcher = {
          def bindParams(pairs: List[(Term, List[Term.Name])]): Term => Term =
            NonEmptyList
              .fromList(pairs)
              .fold[Term => Term](identity)(_.foldLeft[Term => Term](identity) { case (acc, (directive, params)) =>
                params match {
                  case List() =>
                    next => acc(Term.Apply(directive, Term.ArgClause(List(next))))
                  case xs =>
                    next =>
                      acc(
                        Term.Apply(
                          Term.Select(directive, Term.Name("apply")),
                          Term.ArgClause(List(Term.Function(Term.ParamClause(xs.map(x => Term.Param(List.empty, x, None, None))), next)))
                        )
                      )
                }
              })
          val pathMatcher             = bindParams(akkaPath.toList)
          val methodMatcher           = bindParams(List((akkaMethod, List.empty)))
          val customExtractionMatcher = bindParams(customExtractionFields.map(t => (t.term, List(t.param.paramName))).toList)
          val qsMatcher               = bindParams(akkaQs)
          val headerMatcher           = bindParams(akkaHeaders)
          val tracingMatcher          = bindParams(tracingFields.map(t => (t.term, List(t.param.paramName))).toList)
          val bodyMatcher             = bindParams(List((entityProcessor, (bodyArgs.toList ++ formArgs).map(_.paramName))))

          pathMatcher compose methodMatcher compose customExtractionMatcher compose qsMatcher compose headerMatcher compose tracingMatcher compose bodyMatcher
        }
        handlerCallArgs = List(List(responseCompanionTerm)) ++ orderedParameters.map(_.map(_.paramName))
        fullRoute       = Term.Block(List(fullRouteMatcher(q"complete(handler.${Term.Name(methodName)}(...${handlerCallArgs}))")))

        respond = List(List(param"respond: ${Term.Name(resourceName)}.${responseCompanionTerm}.type"))

        params = respond ++ orderedParameters.map(
          _.map(scalaParam =>
            scalaParam.param.copy(
              decltpe = if (scalaParam.isFile) {
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
        codecs <- generateCodecs(methodName, bodyArgs, responses, consumes)
      } yield RenderedRoute(
        fullRoute,
        q"""def ${Term.Name(methodName)}(...${params}): scala.concurrent.Future[${responseType}]""",
        codecs,
        handlerDefinitions
      )
  }

  private def generateHeaderParams(headers: List[Header[ScalaLanguage]], prefix: Term.Name): Term = {
    def liftOptionTerm(tParamName: Term.Name, tName: RawParameterName) =
      q"$prefix.$tParamName.map(v => RawHeader(${tName.toLit}, Formatter.show(v)))"

    def liftTerm(tParamName: Term.Name, tName: RawParameterName) =
      q"Some(RawHeader(${tName.toLit}, Formatter.show($prefix.$tParamName)))"

    val args: List[Term] = headers.map {
      case Header(name, true, _, term) =>
        liftTerm(term, RawParameterName(name))
      case Header(name, false, _, term) =>
        liftOptionTerm(term, RawParameterName(name))
    }
    q"scala.collection.immutable.Seq[Option[HttpHeader]](..$args).flatten"
  }

  private def combineRouteTerms(terms: List[Term]): Target[Term] =
    Target.log.function(s"combineRouteTerms(<${terms.length} routes>)")(for {
      routes <- Target.fromOption(NonEmptyList.fromList(terms), UserError("Generated no routes, no source to generate"))
      _      <- routes.traverse(route => Target.log.debug(route.toString))
    } yield routes.tail.foldLeft(routes.head) { case (a, n) => q"${a} ~ ${n}" })

  private def generateCodecs(
      methodName: String,
      bodyArgs: Option[LanguageParameter[ScalaLanguage]],
      responses: Responses[ScalaLanguage],
      consumes: Tracker[NonEmptyList[ContentType]]
  ): Target[List[Defn.Def]] =
    generateDecoders(methodName, bodyArgs, consumes)

  private def generateDecoders(
      methodName: String,
      bodyArgs: Option[LanguageParameter[ScalaLanguage]],
      consumes: Tracker[NonEmptyList[ContentType]]
  ): Target[List[Defn.Def]] =
    bodyArgs.toList.traverse { case LanguageParameter(_, _, _, _, argType) =>
      for {
        (decoder, baseType) <- AkkaHttpHelper.generateDecoder(argType, consumes, modelGeneratorType)
        decoderImplicits    <- AkkaHttpHelper.protocolImplicits(modelGeneratorType)
      } yield Defn.Def(
        mods = List.empty,
        Term.Name(s"${methodName}Decoder"),
        List(
          Member.ParamClauseGroup(
            Type.ParamClause(Nil),
            List(decoderImplicits).filter(_.nonEmpty)
          )
        ),
        Some(t"FromRequestUnmarshaller[$baseType]"),
        q"""
              val extractEntity = implicitly[Unmarshaller[HttpMessage, HttpEntity]]
              val unmarshalEntity = ${decoder}
              extractEntity.andThen(unmarshalEntity)
            """
      )
    }
}
