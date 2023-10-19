package dev.guardrail.generators.java.springMvc

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.Modifier.Keyword._
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.Node
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import dev.guardrail.AuthImplementation
import dev.guardrail.Context
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.core.extract.ServerRawResponse
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.generators.LanguageParameters
import dev.guardrail.generators.RenderedRoutes
import dev.guardrail.generators.Server
import dev.guardrail.generators.Servers
import dev.guardrail.generators.java.JavaCollectionsGenerator
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.java.JavaVavrCollectionsGenerator
import dev.guardrail.generators.java.SerializationHelpers
import dev.guardrail.generators.java.syntax._
import dev.guardrail.generators.spi.CollectionsGeneratorLoader
import dev.guardrail.generators.spi.ModuleLoadResult
import dev.guardrail.generators.spi.ServerGeneratorLoader
import dev.guardrail.shims._
import dev.guardrail.terms.AnyContentType
import dev.guardrail.terms.ApplicationJson
import dev.guardrail.terms.BinaryContent
import dev.guardrail.terms.CollectionsLibTerms
import dev.guardrail.terms.ContentType
import dev.guardrail.terms.LanguageTerms
import dev.guardrail.terms.MultipartFormData
import dev.guardrail.terms.OctetStream
import dev.guardrail.terms.Response
import dev.guardrail.terms.Responses
import dev.guardrail.terms.RouteMeta
import dev.guardrail.terms.SecurityScheme
import dev.guardrail.terms.SwaggerTerms
import dev.guardrail.terms.TextContent
import dev.guardrail.terms.TextPlain
import dev.guardrail.terms.UrlencodedFormData
import dev.guardrail.terms.collections.CollectionsAbstraction
import dev.guardrail.terms.collections.JavaStdLibCollections
import dev.guardrail.terms.collections.JavaVavrCollections
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.ADT
import dev.guardrail.terms.protocol.ClassDefinition
import dev.guardrail.terms.protocol.EnumDefinition
import dev.guardrail.terms.protocol.RandomType
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.terms.server._
import io.swagger.v3.oas.models.Components
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.responses.ApiResponse

import scala.compat.java8.OptionConverters._
import scala.language.existentials
import scala.reflect.runtime.universe.typeTag
import scala.util.Try

class SpringMvcServerGeneratorLoader extends ServerGeneratorLoader {
  type L = JavaLanguage
  override def reified = typeTag[Target[JavaLanguage]]
  val apply =
    ModuleLoadResult.forProduct3(
      ServerGeneratorLoader.label      -> Seq(SpringMvcVersion.mapping),
      CollectionsGeneratorLoader.label -> Seq(JavaVavrCollectionsGenerator.mapping, JavaCollectionsGenerator.mapping),
      CollectionsGeneratorLoader.label -> Seq(JavaStdLibCollections.mapping, JavaVavrCollections.mapping)
    )((_, cl, ca) => SpringMvcServerGenerator()(cl, ca))
}

object SpringMvcServerGenerator {
  def apply()(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): ServerTerms[JavaLanguage, Target] =
    new SpringMvcServerGenerator
}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Null"))
class SpringMvcServerGenerator private (implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage])
    extends ServerTerms[JavaLanguage, Target] {

  override implicit def MonadF: Monad[Target] = Target.targetInstances

  override def fromSpec(context: Context, supportPackage: NonEmptyList[String], basePath: Option[String], frameworkImports: List[JavaLanguage#Import])(
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[JavaLanguage]],
      securitySchemes: Map[String, SecurityScheme[JavaLanguage]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[Servers[JavaLanguage]] = {
    import Sw._
    import Sc._
    import dev.guardrail._

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
              responses             <- Responses.getResponses[JavaLanguage, Target](operationId, operation, protocolElems, components)
              responseClsName       <- formatTypeName(operationId, Some("Response"))
              responseDefinitions   <- generateResponseDefinitions(responseClsName, responses, protocolElems)
              methodName            <- formatMethodName(operationId)
              parameters            <- route.getParameters[JavaLanguage, Target](components, protocolElems)
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
        } yield Server[JavaLanguage](className, frameworkImports ++ extraImports, handlerSrc, classSrc)
      }
    } yield Servers[JavaLanguage](servers, supportDefinitions)
  }

  @SuppressWarnings(Array("org.wartremover.warts.TripleQuestionMark"))
  private def toSpringMediaType: ContentType => Expression = {
    case _: ApplicationJson    => new FieldAccessExpr(new NameExpr("MediaType"), "APPLICATION_JSON_VALUE")
    case _: UrlencodedFormData => new FieldAccessExpr(new NameExpr("MediaType"), "APPLICATION_FORM_URLENCODED_VALUE")
    case _: MultipartFormData  => new FieldAccessExpr(new NameExpr("MediaType"), "MULTIPART_FORM_DATA_VALUE")
    case _: TextPlain          => new FieldAccessExpr(new NameExpr("MediaType"), "TEXT_PLAIN_VALUE")
    case _: OctetStream        => new FieldAccessExpr(new NameExpr("MediaType"), "APPLICATION_OCTET_STREAM_VALUE")
    case ct: TextContent       => new StringLiteralExpr(ct.value)
    case ct: BinaryContent     => new StringLiteralExpr(ct.value)
    case _: AnyContentType     => ??? // TODO: What do we do if we get here?
  }

  private val ASYNC_RESPONSE_TYPE        = StaticJavaParser.parseClassOrInterfaceType("DeferredResult<ResponseEntity<?>>")
  private val ASYNC_RESPONSE_ERASED_TYPE = StaticJavaParser.parseClassOrInterfaceType("DeferredResult<>")
  private val RESPONSE_TYPE              = StaticJavaParser.parseClassOrInterfaceType("ResponseEntity")
  private val RESPONSE_BUILDER_TYPE      = StaticJavaParser.parseClassOrInterfaceType("ResponseEntity.BodyBuilder")
  private val LOGGER_TYPE                = StaticJavaParser.parseClassOrInterfaceType("Logger")

  private def removeEmpty(s: String): Option[String]       = if (s.trim.isEmpty) None else Some(s.trim)
  private def splitPathComponents(s: String): List[String] = s.split("/").flatMap(removeEmpty).toList

  private def findPathPrefix(routePaths: List[String]): List[String] = {
    def getHeads(sss: List[List[String]]): (List[Option[String]], List[List[String]]) =
      (sss.map(_.headOption), sss.map(_.drop(1)))

    def checkMatch(matching: List[String], headsToCheck: List[Option[String]], restOfHeads: List[List[String]]): List[String] =
      headsToCheck match {
        case Nil => matching
        case x :: xs =>
          x.fold(matching) { first =>
            if (xs.forall(_.contains(first))) {
              val (nextHeads, nextRest) = getHeads(restOfHeads)
              checkMatch(matching :+ first, nextHeads, nextRest)
            } else {
              matching
            }
          }
      }

    val splitRoutePaths             = routePaths.map(splitPathComponents)
    val (initialHeads, initialRest) = getHeads(splitRoutePaths)
    checkMatch(List.empty, initialHeads, initialRest)
  }

  // FIXME: does this handle includes from other files?
  private def definitionName(refName: Option[String]): Option[String] =
    refName.flatMap { rn =>
      rn.split("/").toList match {
        case "#" :: _ :: name :: Nil => Some(name)
        case _                       => None
      }
    }

  def getBestConsumes(contentTypes: List[ContentType], parameters: LanguageParameters[JavaLanguage]): Option[ContentType] = {
    val priorityOrder = NonEmptyList.of(
      UrlencodedFormData.empty,
      ApplicationJson.empty,
      MultipartFormData.empty,
      TextPlain.empty
    )

    priorityOrder
      .foldLeft[Option[ContentType]](None) {
        case (s @ Some(_), _) => s
        case (None, next)     => contentTypes.find(_ == next)
      }
      .orElse(parameters.formParams.headOption.map(_ => UrlencodedFormData.empty))
      .orElse(parameters.bodyParams.map(_ => ApplicationJson.empty))
  }

  private def getBestProduces(
      contentTypes: List[ContentType],
      responses: List[Tracker[ApiResponse]],
      protocolElems: List[StrictProtocolElems[JavaLanguage]]
  ): Option[ContentType] = {
    val priorityOrder = NonEmptyList.of(
      ApplicationJson.empty,
      TextPlain.empty,
      OctetStream.empty
    )

    priorityOrder
      .foldLeft[Option[ContentType]](None) {
        case (s @ Some(_), _) => s
        case (None, next)     => contentTypes.find(_ == next)
      }
      .orElse(
        responses
          .map { resp =>
            protocolElems
              .find(pe => definitionName(resp.downField("ref", _.get$ref()).unwrapTracker).contains(pe.name))
              .flatMap {
                case _: ClassDefinition[_]                                              => Some(ApplicationJson.empty)
                case RandomType(_, tpe) if tpe.isPrimitiveType || tpe.isNamed("String") => Some(TextPlain.empty)
                case _: ADT[_] | _: EnumDefinition[_]                                   => Some(TextPlain.empty)
                case _                                                                  => None
              }
          }
          .headOption
          .flatten
      )
  }

  def generateResponseSuperClass(name: String): Target[ClassOrInterfaceDeclaration] =
    Target.log.function("generateResponseSuperClass") {
      for {
        _ <- Target.log.info(s"Name: ${name}")
        cls = new ClassOrInterfaceDeclaration(new NodeList(abstractModifier), false, name)

        _ = cls.addField(PrimitiveType.intType, "statusCode", PRIVATE, FINAL)

        _ = cls
          .addConstructor()
          .addParameter(new Parameter(new NodeList(finalModifier), PrimitiveType.intType, new SimpleName("statusCode")))
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "statusCode"), new NameExpr("statusCode"), AssignExpr.Operator.ASSIGN))
              )
            )
          )

        _ = cls
          .addMethod(s"getStatusCode", PUBLIC)
          .setType(PrimitiveType.intType)
          .setBody(
            new BlockStmt(
              new NodeList(
                new ReturnStmt(new FieldAccessExpr(new ThisExpr, "statusCode"))
              )
            )
          )
      } yield cls
    }

  def generateResponseClass(
      superClassType: ClassOrInterfaceType,
      response: Response[JavaLanguage],
      errorEntityFallbackType: Option[Type]
  ): Target[(ClassOrInterfaceDeclaration, BodyDeclaration[_ <: BodyDeclaration[_]])] = {
    val clsName = response.statusCodeName.asString
    for {
      clsType <- safeParseClassOrInterfaceType(clsName)
    } yield {
      val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, clsName)
        .setExtendedTypes(new NodeList(superClassType))

      val (classDecls, creator) = response.value
        .map(_._2)
        .orElse {
          if (response.statusCode >= 400 && response.statusCode <= 599) {
            errorEntityFallbackType
          } else {
            None
          }
        }
        .fold[(List[BodyDeclaration[_ <: BodyDeclaration[_]]], BodyDeclaration[_ <: BodyDeclaration[_]])] {
          val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
          val _ = constructor.setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(
                  new MethodCallExpr(
                    "super",
                    new IntegerLiteralExpr(response.statusCode.toString)
                  )
                )
              )
            )
          )

          val creator = new FieldDeclaration(
            new NodeList(publicModifier, staticModifier, finalModifier),
            new VariableDeclarator(clsType, clsName, new ObjectCreationExpr(null, clsType, new NodeList))
          )

          (List(constructor), creator)
        } { valueType =>
          val constructParam = new Parameter(new NodeList(finalModifier), valueType.unbox, new SimpleName("entityBody"))

          val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
            .addParameter(constructParam)
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new MethodCallExpr(
                      "super",
                      new IntegerLiteralExpr(response.statusCode.toString)
                    )
                  ),
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, constructParam.getNameAsString),
                      constructParam.getNameAsExpression,
                      AssignExpr.Operator.ASSIGN
                    )
                  )
                )
              )
            )

          val entityBodyField = new FieldDeclaration(
            new NodeList(privateModifier, finalModifier),
            new VariableDeclarator(valueType, "entityBody")
          )

          val entityBodyGetter = new MethodDeclaration(new NodeList(publicModifier), valueType, "getEntityBody")
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new FieldAccessExpr(new ThisExpr, "entityBody"))
                )
              )
            )

          val creator = new MethodDeclaration(new NodeList(publicModifier, staticModifier), clsType, clsName)
            .addParameter(constructParam)
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new ObjectCreationExpr(null, clsType, new NodeList(constructParam.getNameAsExpression)))
                )
              )
            )

          (List(constructor, entityBodyField, entityBodyGetter), creator)
        }

      sortDefinitions(classDecls).foreach(cls.addMember)
      (cls, creator)
    }
  }

  private def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]) =
    List(
      "java.util.Optional",
      "java.util.concurrent.CompletionStage",
      "javax.validation.constraints.NotNull",
      "org.slf4j.Logger",
      "org.slf4j.LoggerFactory",
      "org.springframework.beans.factory.annotation.Autowired",
      "org.springframework.format.annotation.DateTimeFormat",
      "org.springframework.http.HttpStatus",
      "org.springframework.http.MediaType",
      "org.springframework.http.ResponseEntity",
      "org.springframework.web.bind.annotation.*",
      "org.springframework.web.context.request.async.DeferredResult",
      "org.springframework.web.multipart.MultipartFile"
    ).traverse(safeParseRawImport)

  private def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean) =
    if (customExtraction) {
      Target.raiseUserError(s"Custom Extraction is not yet supported by this framework")
    } else {
      Target.pure(Option.empty)
    }

  private def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean) =
    if (tracing) {
      Target.raiseUserError(s"Tracing is not yet supported by this framework")
    } else {
      Target.pure(Option.empty)
    }

  private def generateRoutes(
      tracing: Boolean,
      resourceName: String,
      handlerName: String,
      basePath: Option[String],
      routes: List[GenerateRouteMeta[JavaLanguage]],
      protocolElems: List[StrictProtocolElems[JavaLanguage]],
      securitySchemes: Map[String, SecurityScheme[JavaLanguage]],
      securityExposure: SecurityExposure,
      authImplementation: AuthImplementation
  ) =
    for {
      resourceType <- safeParseClassOrInterfaceType(resourceName)
      handlerType  <- safeParseClassOrInterfaceType(handlerName)
    } yield {
      import Ca._
      val basePathComponents = basePath.toList.flatMap(splitPathComponents)
      val commonPathPrefix   = findPathPrefix(routes.map(_.routeMeta.path.unwrapTracker))

      val (routeMethods, handlerMethodSigs) = routes.map {
        case GenerateRouteMeta(
              operationId,
              methodName,
              responseClsName,
              customExtractionFields,
              tracingFields,
              sr @ RouteMeta(path, httpMethod, operation, securityRequirements),
              parameters,
              responses
            ) =>
          parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))

          val httpMethodAnnotationName = s"${httpMethod.toString.toLowerCase.capitalize}Mapping"
          val pathSuffix = splitPathComponents(path.unwrapTracker)
            .drop(commonPathPrefix.length)
            .mkString("/", "/", "")

          val method   = new MethodDeclaration(new NodeList(publicModifier), ASYNC_RESPONSE_TYPE, methodName)
          val nodeList = new NodeList[MemberValuePair]()
          if (pathSuffix.nonEmpty && pathSuffix != "/") {
            nodeList.addLast(new MemberValuePair("path", new StringLiteralExpr(pathSuffix)))
          }

          val consumes = getBestConsumes(operation.unwrapTracker.consumes.flatMap(ContentType.unapply).toList, parameters)
            .orElse {
              if (parameters.formParams.nonEmpty) {
                if (parameters.formParams.exists(_.isFile)) {
                  Some(MultipartFormData.empty)
                } else {
                  Some(UrlencodedFormData.empty)
                }
              } else if (parameters.bodyParams.nonEmpty) {
                Some(ApplicationJson.empty)
              } else {
                None
              }
            }

          consumes
            .map(c => new MemberValuePair("consumes", toSpringMediaType(c)))
            .foreach(nodeList.addLast)

          val successResponses =
            operation
              .downField("responses", _.getResponses)
              .indexedDistribute
              .value
              .filter { case (key, _) => Try(key.toInt / 100 == 2).getOrElse(false) }
              .map { case (_, value) => value }
              .toList
          val produces = getBestProduces(operation.unwrapTracker.produces.flatMap(ContentType.unapply).toList, successResponses, protocolElems)

          produces
            .map(c => new MemberValuePair("produces", toSpringMediaType(c)))
            .foreach(nodeList.addLast)

          if (!nodeList.isEmpty) {
            method.addAnnotation(
              new NormalAnnotationExpr(
                new Name(httpMethodAnnotationName),
                nodeList
              )
            )
          } else {
            method.addAnnotation(new MarkerAnnotationExpr(new Name(httpMethodAnnotationName)))
          }

          def transformJsr310Params(parameter: Parameter): Parameter = {
            val isOptional = parameter.getType.isOptionalType
            val tpe        = if (isOptional) parameter.getType.containedType else parameter.getType
            def transform(dateTimeFormat: String): Parameter = {
              parameter.getAnnotations.addLast(
                new NormalAnnotationExpr(
                  new Name("DateTimeFormat"),
                  new NodeList(
                    new MemberValuePair(
                      "iso",
                      new FieldAccessExpr(new NameExpr("DateTimeFormat.ISO"), dateTimeFormat)
                    )
                  )
                )
              )
              parameter
            }

            tpe match {
              case cls: ClassOrInterfaceType if cls.getScope.asScala.forall(_.asString == "java.time") =>
                cls.getNameAsString match {
                  case "Instant"        => parameter
                  case "OffsetDateTime" => transform("DATE_TIME")
                  case "ZonedDateTime"  => transform("DATE_TIME")
                  case "LocalDateTime"  => transform("DATE_TIME")
                  case "LocalDate"      => transform("DATE")
                  case "LocalTime"      => transform("TIME")
                  case "OffsetTime"     => transform("TIME")
                  case "Duration"       => parameter
                  case _                => parameter
                }
              case _ => parameter
            }
          }

          def addValidationAnnotations(parameter: Parameter, param: LanguageParameter[JavaLanguage]): Parameter = {
            if (param.required) {
              parameter.getAnnotations.add(0, new MarkerAnnotationExpr("NotNull"))
            }
            parameter
          }

          def addParamAnnotation(parameter: Parameter, param: LanguageParameter[JavaLanguage], annotationName: String): Parameter =
            parameter.addAnnotation(new SingleMemberAnnotationExpr(new Name(annotationName), new StringLiteralExpr(param.argName.value)))

          def addParamMarkerAnnotation(parameter: Parameter, param: LanguageParameter[JavaLanguage], annotationName: String): Parameter =
            parameter.addAnnotation(new MarkerAnnotationExpr(new Name(annotationName)))

          def boxParameterTypes(parameter: Parameter): Parameter = {
            if (parameter.getType.isPrimitiveType) {
              parameter.setType(parameter.getType.asPrimitiveType.toBoxedType)
            }
            parameter
          }

          val annotatedMethodParams: List[Parameter] = List(
            (parameters.pathParams, "PathVariable"),
            (parameters.headerParams, "RequestHeader"),
            (parameters.queryStringParams, "RequestParam"),
            (parameters.formParams, if (consumes.exists(ContentType.isSubtypeOf[MultipartFormData])) "RequestParam" else "ModelAttribute")
          ).flatMap { case (params, annotationName) =>
            params.map { param =>
              val parameter       = param.param.clone()
              val annotated       = addParamAnnotation(parameter, param, annotationName)
              val dateTransformed = transformJsr310Params(annotated)
              addValidationAnnotations(dateTransformed, param)
            }
          }

          val bareMethodParams: List[Parameter] = parameters.bodyParams.toList
            .map { param =>
              val parameter       = param.param.clone()
              val annotated       = addParamMarkerAnnotation(parameter, param, "RequestBody")
              val dateTransformed = transformJsr310Params(annotated)
              addValidationAnnotations(dateTransformed, param)
            }

          val methodParams = (annotatedMethodParams ++ bareMethodParams).map(boxParameterTypes)
          methodParams.foreach(method.addParameter)

          val hasServerRawResponse: Option[Boolean] = ServerRawResponse(operation).filter(_ == true)

          val (responseName, responseType, resultResumeBody) =
            hasServerRawResponse.fold {
              val responseName = s"$handlerName.$responseClsName"
              val responseType = StaticJavaParser.parseClassOrInterfaceType(responseName)
              val entitySetterIfTree = NonEmptyList
                .fromList(responses.value.collect { case Response(statusCodeName, Some(_), _) =>
                  statusCodeName
                })
                .map(_.reverse.foldLeft[IfStmt](null) { case (nextIfTree, statusCodeName) =>
                  val responseSubclassType = StaticJavaParser.parseClassOrInterfaceType(
                    s"$responseName.$statusCodeName"
                  )
                  new IfStmt(
                    new InstanceOfExpr(new NameExpr("result"), responseSubclassType),
                    new BlockStmt(
                      new NodeList(
                        new ExpressionStmt(
                          new MethodCallExpr(
                            new NameExpr("response"),
                            "setResult",
                            new NodeList[Expression](
                              new MethodCallExpr(
                                new NameExpr("builder"),
                                "body",
                                new NodeList[Expression](
                                  new MethodCallExpr(
                                    new EnclosedExpr(new CastExpr(responseSubclassType, new NameExpr("result"))),
                                    "getEntityBody"
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    nextIfTree
                  )
                })

              val finalizeBody =
                new ExpressionStmt(
                  new MethodCallExpr(
                    new NameExpr("response"),
                    "setResult",
                    new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build"))
                  )
                )

              val settingBody: Statement = entitySetterIfTree match {
                case Some(ifStmt: IfStmt) =>
                  ifStmt.setElseStmt(
                    new BlockStmt(
                      new NodeList[Statement](finalizeBody)
                    )
                  )
                  ifStmt
                case None => finalizeBody
              }

              (
                responseName,
                responseType,
                List[Statement](
                  new ExpressionStmt(
                    new VariableDeclarationExpr(
                      new VariableDeclarator(
                        RESPONSE_BUILDER_TYPE,
                        "builder",
                        new MethodCallExpr(
                          new NameExpr("ResponseEntity"),
                          "status",
                          new NodeList[Expression](new MethodCallExpr(new NameExpr("result"), "getStatusCode"))
                        )
                      ),
                      finalModifier
                    )
                  ),
                  settingBody
                ).toNodeList
              )
            } { _ =>
              (
                "Response",
                RESPONSE_TYPE,
                new NodeList(
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new NameExpr("response"),
                      "setResult",
                      new NodeList[Expression](new NameExpr("result"))
                    )
                  )
                )
              )
            }

          val whenCompleteLambda = new LambdaExpr(
            new NodeList(
              new Parameter(new NodeList(finalModifier), responseType, new SimpleName("result")),
              new Parameter(new NodeList(finalModifier), THROWABLE_TYPE, new SimpleName("err"))
            ),
            new BlockStmt(
              new NodeList(
                new IfStmt(
                  new BinaryExpr(new NameExpr("err"), new NullLiteralExpr, BinaryExpr.Operator.NOT_EQUALS),
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(
                        new MethodCallExpr(
                          new NameExpr("logger"),
                          "error",
                          new NodeList[Expression](
                            new StringLiteralExpr(s"${handlerName}.${methodName} threw an exception ({}): {}"),
                            new MethodCallExpr(new MethodCallExpr(new NameExpr("err"), "getClass"), "getName"),
                            new MethodCallExpr(new NameExpr("err"), "getMessage"),
                            new NameExpr("err")
                          )
                        )
                      ),
                      new ExpressionStmt(
                        new MethodCallExpr(
                          new NameExpr("response"),
                          "setErrorResult",
                          new NodeList[Expression](
                            new MethodCallExpr(
                              new MethodCallExpr(
                                new NameExpr("ResponseEntity"),
                                "status",
                                new NodeList[Expression](new FieldAccessExpr(new NameExpr("HttpStatus"), "INTERNAL_SERVER_ERROR"))
                              ),
                              "build"
                            )
                          )
                        )
                      )
                    )
                  ),
                  new BlockStmt(resultResumeBody)
                )
              )
            ),
            true
          )

          def transformHandlerArg(parameter: Parameter): Expression =
            parameter.getNameAsExpression

          val handlerCall = new MethodCallExpr(
            new FieldAccessExpr(new ThisExpr, "handler"),
            methodName,
            new NodeList[Expression](methodParams.map(transformHandlerArg): _*)
          )

          method.setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(
                  new VariableDeclarationExpr(
                    new VariableDeclarator(ASYNC_RESPONSE_TYPE, "response", new ObjectCreationExpr(null, ASYNC_RESPONSE_ERASED_TYPE, new NodeList))
                  )
                ),
                new ExpressionStmt(new MethodCallExpr(handlerCall, "whenComplete", new NodeList[Expression](whenCompleteLambda))),
                new ReturnStmt("response")
              )
            )
          )

          val futureResponseType = responseType.liftFutureType
          val handlerMethodSig   = new MethodDeclaration(new NodeList(), futureResponseType, methodName)
          (parameters.pathParams ++ parameters.headerParams ++ parameters.queryStringParams ++ parameters.formParams ++ parameters.bodyParams).foreach {
            parameter =>
              handlerMethodSig.addParameter(parameter.param.clone())
          }
          handlerMethodSig.setBody(null)

          (method, handlerMethodSig)
      }.unzip

      val resourceConstructor = new ConstructorDeclaration(new NodeList(publicModifier), resourceName)
      resourceConstructor.addAnnotation(new MarkerAnnotationExpr(new Name("Autowired")))
      resourceConstructor.addParameter(new Parameter(new NodeList(finalModifier), handlerType, new SimpleName("handler")))
      resourceConstructor.setBody(
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "handler"), new NameExpr("handler"), AssignExpr.Operator.ASSIGN))
          )
        )
      )

      val annotations = List(
        new MarkerAnnotationExpr(new Name("RestController")),
        new SingleMemberAnnotationExpr(new Name("RequestMapping"), new StringLiteralExpr((basePathComponents ++ commonPathPrefix).mkString("/", "/", "")))
      )

      val supportDefinitions = List[BodyDeclaration[_ <: BodyDeclaration[_]]](
        new FieldDeclaration(
          new NodeList(privateModifier, staticModifier, finalModifier),
          new VariableDeclarator(
            LOGGER_TYPE,
            "logger",
            new MethodCallExpr(new NameExpr("LoggerFactory"), "getLogger", new NodeList[Expression](new ClassExpr(resourceType)))
          )
        ),
        new FieldDeclaration(new NodeList(privateModifier, finalModifier), new VariableDeclarator(handlerType, "handler")),
        resourceConstructor
      )

      RenderedRoutes[JavaLanguage](routeMethods, annotations, handlerMethodSigs, supportDefinitions, List.empty, List.empty)
    }

  private def getExtraRouteParams(
      resourceName: String,
      customExtraction: Boolean,
      tracing: Boolean,
      authImplementation: AuthImplementation,
      securityExposure: SecurityExposure
  ) =
    for {
      customExtraction <-
        if (customExtraction) {
          Target.raiseUserError(s"Custom Extraction is not yet supported by this framework")
        } else Target.pure(List.empty)

      tracing <-
        if (tracing) {
          Target.raiseUserError(s"Tracing is not yet supported by this framework")
        } else Target.pure(List.empty)
    } yield customExtraction ::: tracing

  private def generateResponseDefinitions(
      responseClsName: String,
      responses: Responses[JavaLanguage],
      protocolElems: List[StrictProtocolElems[JavaLanguage]]
  ) =
    for {
      abstractResponseClassType <- safeParseClassOrInterfaceType(responseClsName)

      // TODO: verify valueTypes are in protocolElems

      abstractResponseClass <- generateResponseSuperClass(responseClsName)
      responseClasses       <- responses.value.traverse(resp => generateResponseClass(abstractResponseClassType, resp, None))
    } yield {
      sortDefinitions(responseClasses.flatMap { case (cls, creator) => List[BodyDeclaration[_ <: BodyDeclaration[_]]](cls, creator) })
        .foreach(abstractResponseClass.addMember)
      abstractResponseClass :: Nil
    }

  private def generateSupportDefinitions(
      tracing: Boolean,
      securitySchemes: Map[String, SecurityScheme[JavaLanguage]]
  ) =
    for {
      shower <- SerializationHelpers.showerSupportDef
    } yield List(shower)

  private def renderClass(
      className: String,
      handlerName: String,
      classAnnotations: List[com.github.javaparser.ast.expr.AnnotationExpr],
      combinedRouteTerms: List[com.github.javaparser.ast.Node],
      extraRouteParams: List[com.github.javaparser.ast.body.Parameter],
      responseDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
      supportDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
      securitySchemesDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
      customExtraction: Boolean,
      authImplementation: AuthImplementation
  ) =
    safeParseSimpleName(className) >>
      safeParseSimpleName(handlerName) >>
      Target.pure(doRenderClass(className, classAnnotations, supportDefinitions, combinedRouteTerms) :: Nil)

  private def renderHandler(
      handlerName: String,
      methodSigs: List[com.github.javaparser.ast.body.MethodDeclaration],
      handlerDefinitions: List[com.github.javaparser.ast.Node],
      responseDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
      customExtraction: Boolean,
      authImplementation: AuthImplementation,
      securityExposure: SecurityExposure
  ) = {
    val handlerClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), true, handlerName)
    sortDefinitions(methodSigs ++ responseDefinitions).foreach(handlerClass.addMember)
    Target.pure(handlerClass)
  }

  // Lift this function out of RenderClass above to work around a 2.11.x compiler syntax bug
  private def doRenderClass(
      className: String,
      classAnnotations: List[AnnotationExpr],
      supportDefinitions: List[BodyDeclaration[_ <: BodyDeclaration[_]]],
      combinedRouteTerms: List[Node]
  ): ClassOrInterfaceDeclaration = {
    val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, className)
    classAnnotations.foreach(cls.addAnnotation)
    sortDefinitions(supportDefinitions ++ combinedRouteTerms.collect { case bd: BodyDeclaration[_] => bd })
      .foreach(cls.addMember)
    cls
  }
}
