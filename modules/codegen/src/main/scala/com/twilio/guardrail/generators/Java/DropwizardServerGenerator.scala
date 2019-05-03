package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.~>
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.{ Modifier, Node, NodeList }
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, TypeParameter, VoidType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.{ ADT, ClassDefinition, EnumDefinition, RandomType, RenderedRoutes, StrictProtocolElems, SupportDefinition, Target, TracingField }
import com.twilio.guardrail.extract.{ SecurityOptional, ServerRawResponse }
import com.twilio.guardrail.generators.ScalaParameters
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.{ Response, Responses }
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.shims.OperationExt
import com.twilio.guardrail.terms.{ ApiKeySecurityScheme, HttpSecurityScheme, OAuth2SecurityScheme, OpenIdConnectSecurityScheme, RouteMeta, SecurityScheme }
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import java.util
import java.util.Locale
import scala.collection.JavaConverters._
import scala.language.existentials
import scala.util.Try

object DropwizardServerGenerator {
  private implicit class ContentTypeExt(val ct: RouteMeta.ContentType) extends AnyVal {
    def toJaxRsAnnotationName: String = ct match {
      case RouteMeta.ApplicationJson    => "APPLICATION_JSON"
      case RouteMeta.UrlencodedFormData => "APPLICATION_FORM_URLENCODED"
      case RouteMeta.MultipartFormData  => "MULTIPART_FORM_DATA"
      case RouteMeta.TextPlain          => "TEXT_PLAIN"
    }
  }

  private val ASYNC_RESPONSE_TYPE   = JavaParser.parseClassOrInterfaceType("AsyncResponse")
  private val RESPONSE_TYPE         = JavaParser.parseClassOrInterfaceType("Response")
  private val RESPONSE_BUILDER_TYPE = JavaParser.parseClassOrInterfaceType("Response.ResponseBuilder")
  private val LOGGER_TYPE           = JavaParser.parseClassOrInterfaceType("Logger")

  private val PRINCIPAL_TYPE = JavaParser.parseClassOrInterfaceType("Principal")

  private val GENERIC_A_TYPE                   = JavaParser.parseClassOrInterfaceType("A")
  private def apiKeyAuthFilterType(of: Type) = JavaParser.parseClassOrInterfaceType("ApiKeyAuthFilter").setTypeArguments(of)

  private def apiKeyAuthFilterBuilderType(a: Type, b: Type) = JavaParser.parseClassOrInterfaceType("ApiKeyAuthFilterBuilder").setTypeArguments(a, b)

  private def removeEmpty(s: String): Option[String]       = if (s.trim.isEmpty) None else Some(s.trim)
  private def splitPathComponents(s: String): List[String] = s.split("/").flatMap(removeEmpty).toList

  private def findPathPrefix(routePaths: List[String]): List[String] = {
    def getHeads(sss: List[List[String]]): (List[Option[String]], List[List[String]]) =
      (sss.map(_.headOption), sss.map(ss => ss.headOption.fold(List.empty[String])(_ => ss.tail)))

    def checkMatch(matching: List[String], headsToCheck: List[Option[String]], restOfHeads: List[List[String]]): List[String] =
      headsToCheck.headOption
        .flatMap({ firstO =>
          firstO.map({ first =>
            if (headsToCheck.tail.forall(_.contains(first))) {
              val (nextHeads, nextRest) = getHeads(restOfHeads)
              checkMatch(matching :+ first, nextHeads, nextRest)
            } else {
              matching
            }
          })
        })
        .getOrElse(matching)

    val splitRoutePaths             = routePaths.map(splitPathComponents)
    val (initialHeads, initialRest) = getHeads(splitRoutePaths)
    checkMatch(List.empty, initialHeads, initialRest)
  }

  // FIXME: does this handle includes from other files?
  private def definitionName(refName: Option[String]): Option[String] =
    refName.flatMap({ rn =>
      rn.split("/").toList match {
        case "#" :: _ :: name :: Nil => Some(name)
        case _                       => None
      }
    })

  def getBestConsumes(contentTypes: List[RouteMeta.ContentType], parameters: ScalaParameters[JavaLanguage]): Option[RouteMeta.ContentType] = {
    val priorityOrder = NonEmptyList.of(
      RouteMeta.UrlencodedFormData,
      RouteMeta.ApplicationJson,
      RouteMeta.MultipartFormData,
      RouteMeta.TextPlain
    )

    priorityOrder
      .foldLeft[Option[RouteMeta.ContentType]](None)({
        case (s @ Some(_), _) => s
        case (None, next)     => contentTypes.find(_ == next)
      })
      .orElse(parameters.formParams.headOption.map(_ => RouteMeta.UrlencodedFormData))
      .orElse(parameters.bodyParams.map(_ => RouteMeta.ApplicationJson))
  }

  private def getBestProduces(contentTypes: List[RouteMeta.ContentType],
                              responses: List[ApiResponse],
                              protocolElems: List[StrictProtocolElems[JavaLanguage]]): Option[RouteMeta.ContentType] = {
    val priorityOrder = NonEmptyList.of(
      RouteMeta.ApplicationJson,
      RouteMeta.TextPlain
    )

    priorityOrder
      .foldLeft[Option[RouteMeta.ContentType]](None)({
        case (s @ Some(_), _) => s
        case (None, next)     => contentTypes.find(_ == next)
      })
      .orElse(
        responses
          .map({ resp =>
            protocolElems
              .find(pe => definitionName(Option(resp.get$ref())).contains(pe.name))
              .flatMap({
                case _: ClassDefinition[_]                                              => Some(RouteMeta.ApplicationJson)
                case RandomType(_, tpe) if tpe.isPrimitiveType || tpe.isNamed("String") => Some(RouteMeta.TextPlain)
                case _: ADT[_] | _: EnumDefinition[_]                                   => Some(RouteMeta.TextPlain)
                case _                                                                  => None
              })
          })
          .headOption
          .flatten
      )
  }

  case class SecurityParameters(methodSuffix: String,
                                routeParameters: List[Parameter],
                                routeStatements: List[Statement],
                                principals: List[(Type, String)])

  type UnknownHttpAuthSchemeHandler = (Operation, String, HttpSecurityScheme, List[String]) => Target[SecurityParameters]

  def emptyUnknownHttpAuthSchemeHandler(operation: Operation,
                                        schemeName: String,
                                        securityScheme: HttpSecurityScheme,
                                        scopes: List[String]): Target[SecurityParameters] =
    Target.raiseError(s"HTTP auth scheme ${securityScheme.authScheme} is not yet supported")

  def generateSecurityParams(operation: Operation,
                             securitySchemes: Map[String, SecurityScheme],
                             unknownHttpAuthSchemeHandler: UnknownHttpAuthSchemeHandler): Target[List[SecurityParameters]] = {
    val optionalRequirements = SecurityOptional(operation)
    val security = Option(operation.getSecurity).toList.flatMap(_.asScala)
    security
      .traverse({ requirement =>
        requirement.asScala.toList.traverse({
          case (schemeName, scopes) =>
            securitySchemes
              .get(schemeName)
              .fold(
                Target.raiseError[SecurityParameters](s"Operation '${operation.getOperationId} references undefined security scheme $schemeName")
              )({ scheme =>
                def createParameters(authPrincipalTypePrefix: String): Target[SecurityParameters] =
                  for {
                    authPrincipalType <- safeParseClassOrInterfaceType(s"${authPrincipalTypePrefix}AuthPrincipal")
                    authPrincipalTypeArg <- scheme.typeName.fold(Target.pure(STRING_TYPE))(safeParseClassOrInterfaceType)
                  } yield {
                    val parameterName = s"${schemeName.toCamelCase}Principal"
                    val rawParameterType = authPrincipalType.setTypeArguments(authPrincipalTypeArg)
                    val parameterType = if (optionalRequirements.contains(schemeName)) {
                      optionalType(rawParameterType)
                    } else {
                      rawParameterType
                    }
                    val routeParameter = new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName))
                      .addMarkerAnnotation("Auth")
                    SecurityParameters(schemeName.toPascalCase, List(routeParameter), List.empty, List((parameterType, parameterName)))
                  }

                scheme match {
                  case ApiKeySecurityScheme(_, SwSecurityScheme.In.QUERY, _, _) => createParameters("ApiKeyQuery")
                  case ApiKeySecurityScheme(_, SwSecurityScheme.In.HEADER, _, _) => createParameters("ApiKeyHeader")
                  case ApiKeySecurityScheme(_, SwSecurityScheme.In.COOKIE, _, _) => createParameters("ApiKeyCookie")
                  case HttpSecurityScheme("basic", _, _) => createParameters("HttpBasic")
                  case HttpSecurityScheme("bearer", _, _) => createParameters("HttpBearer")
                  case httpScheme: HttpSecurityScheme => unknownHttpAuthSchemeHandler(operation, schemeName, httpScheme, scopes.asScala.toList)
                  case _: OAuth2SecurityScheme => createParameters("OAuth")
                  case _: OpenIdConnectSecurityScheme => createParameters("OpenIdConnect")
                }
              })
        }).map(_.foldLeft(SecurityParameters("", List.empty, List.empty, List.empty))(
          (accum, next) => SecurityParameters(
            methodSuffix = accum.methodSuffix + next.methodSuffix,
            routeParameters = accum.routeParameters ++ next.routeParameters,
            routeStatements = accum.routeStatements ++ next.routeStatements,
            principals = accum.principals ++ next.principals
          )
        ))
      })
  }

  def generateResponseSuperClass(name: String): Target[ClassOrInterfaceDeclaration] = {
    val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(ABSTRACT), false, name)
    cls.addField(PrimitiveType.intType, "statusCode", PRIVATE, FINAL)

    cls
      .addConstructor()
      .addParameter(new Parameter(util.EnumSet.of(FINAL), PrimitiveType.intType, new SimpleName("statusCode")))
      .setBody(
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "statusCode"), new NameExpr("statusCode"), AssignExpr.Operator.ASSIGN))
          )
        )
      )

    cls
      .addMethod(s"getStatusCode", PUBLIC)
      .setType(PrimitiveType.intType)
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new FieldAccessExpr(new ThisExpr, "statusCode"))
          )
        )
      )

    Target.pure(cls)
  }

  def generateResponseClass(superClassType: ClassOrInterfaceType,
                            response: Response[JavaLanguage],
                            errorEntityFallbackType: Option[Type]): Target[(ClassOrInterfaceDeclaration, BodyDeclaration[_ <: BodyDeclaration[_]])] = {
    val clsName = response.statusCodeName.asString
    for {
      clsType <- safeParseClassOrInterfaceType(clsName)
    } yield {
      val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, clsName)
        .setExtendedTypes(new NodeList(superClassType))

      val (classDecls, creator) = response.value
        .map(_._1)
        .orElse({
          if (response.statusCode >= 400 && response.statusCode <= 599) {
            errorEntityFallbackType
          } else {
            None
          }
        })
        .fold[(List[BodyDeclaration[_ <: BodyDeclaration[_]]], BodyDeclaration[_ <: BodyDeclaration[_]])]({
          val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
          constructor.setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(
                  new MethodCallExpr(
                    "super",
                    new IntegerLiteralExpr(response.statusCode)
                  )
                )
              )
            )
          )

          val creator = new FieldDeclaration(
            util.EnumSet.of(PUBLIC, STATIC, FINAL),
            new VariableDeclarator(clsType, clsName, new ObjectCreationExpr(null, clsType, new NodeList))
          )

          (List(constructor), creator)
        })({ valueType =>
          val constructParam = new Parameter(util.EnumSet.of(FINAL), valueType.unbox, new SimpleName("entityBody"))

          val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
            .addParameter(constructParam)
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new MethodCallExpr(
                      "super",
                      new IntegerLiteralExpr(response.statusCode)
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
            util.EnumSet.of(PRIVATE, FINAL),
            new VariableDeclarator(valueType, "entityBody")
          )

          val entityBodyGetter = new MethodDeclaration(util.EnumSet.of(PUBLIC), valueType, "getEntityBody")
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new FieldAccessExpr(new ThisExpr, "entityBody"))
                )
              )
            )

          val creator = new MethodDeclaration(util.EnumSet.of(PUBLIC, STATIC), clsType, clsName)
            .addParameter(constructParam)
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new ObjectCreationExpr(null, clsType, new NodeList(constructParam.getNameAsExpression)))
                )
              )
            )

          (List(constructor, entityBodyField, entityBodyGetter), creator)
        })

      sortDefinitions(classDecls).foreach(cls.addMember)
      (cls, creator)
    }
  }

  def generateRoute(operation: Operation,
                    path: String,
                    commonPathPrefix: List[String],
                    httpMethod: HttpMethod,
                    parameters: ScalaParameters[JavaLanguage],
                    responses: Responses[JavaLanguage],
                    protocolElems: List[StrictProtocolElems[JavaLanguage]],
                    securityParameters: List[SecurityParameters],
                    unknownHttpAuthSchemeHandler: UnknownHttpAuthSchemeHandler,
                    handlerName: String): Target[(List[MethodDeclaration], MethodDeclaration)] = {
    val methodName = operation.getOperationId.toCamelCase
    val pathSuffix = splitPathComponents(path).drop(commonPathPrefix.length).mkString("/", "/", "")
    parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))

    val consumes = getBestConsumes(operation.consumes.flatMap(RouteMeta.ContentType.unapply).toList, parameters)
      .orElse({
        if (parameters.formParams.nonEmpty) {
          if (parameters.formParams.exists(_.isFile)) {
            Some(RouteMeta.MultipartFormData)
          } else {
            Some(RouteMeta.UrlencodedFormData)
          }
        } else if (parameters.bodyParams.nonEmpty) {
          Some(RouteMeta.ApplicationJson)
        } else {
          None
        }
      })

    val successResponses =
      operation.getResponses.entrySet.asScala.filter(entry => Try(entry.getKey.toInt / 100 == 2).getOrElse(false)).map(_.getValue).toList
    val produces = getBestProduces(operation.produces.flatMap(RouteMeta.ContentType.unapply).toList, successResponses, protocolElems)

    val methodParams: List[Parameter] = List(
      (parameters.pathParams, "PathParam"),
      (parameters.headerParams, "HeaderParam"),
      (parameters.queryStringParams, "QueryParam"),
      (parameters.formParams, if (consumes.contains(RouteMeta.MultipartFormData)) "FormDataParam" else "FormParam")
    ).flatMap({
      case (params, annotationName) =>
        params.map({ param =>
          param.param.clone()
            .addAnnotation(new SingleMemberAnnotationExpr(new Name(annotationName), new StringLiteralExpr(param.argName.value)))
        })
    }) ++ parameters.bodyParams.map(_.param)

    val (responseName, responseType) = ServerRawResponse(operation)
      .filter(_ == true)
      .fold({
        val name = s"${handlerName}.${methodName.capitalize}Response"
        (name, JavaParser.parseClassOrInterfaceType(name))
      })(_ => (RESPONSE_TYPE.getName.asString, RESPONSE_TYPE))

    val handlerNeedsPrincipalList = securityParameters.map(_.principals).nonEmpty

    val methodVariants = NonEmptyList
      .fromList(securityParameters)
      .getOrElse(NonEmptyList.one(SecurityParameters("", List.empty, List.empty, List.empty)))

    val routeMethods = methodVariants.map({ securityParams =>
      val method = new MethodDeclaration(util.EnumSet.of(PUBLIC), new VoidType, methodName + securityParams.methodSuffix)
        .addAnnotation(new MarkerAnnotationExpr(httpMethod.toString))

      if (pathSuffix.nonEmpty && pathSuffix != "/") {
        method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr(pathSuffix)))
      }

      consumes
        .map(c => new SingleMemberAnnotationExpr(new Name("Consumes"), new FieldAccessExpr(new NameExpr("MediaType"), c.toJaxRsAnnotationName)))
        .foreach(method.addAnnotation)

      produces
        .map(p => new SingleMemberAnnotationExpr(new Name("Produces"), new FieldAccessExpr(new NameExpr("MediaType"), p.toJaxRsAnnotationName)))
        .foreach(method.addAnnotation)

      securityParams.routeParameters.foreach(method.addParameter)
      methodParams.map(_.clone()).foreach(method.addParameter)
      method.addParameter(
        new Parameter(util.EnumSet.of(FINAL), ASYNC_RESPONSE_TYPE, new SimpleName("asyncResponse")).addMarkerAnnotation("Suspended")
      )

      val resultResumeBody =
        ServerRawResponse(operation)
          .filter(_ == true)
          .fold({
            val entitySetterIfTree = NonEmptyList
              .fromList(responses.value.collect({
                case Response(statusCodeName, Some(_)) => statusCodeName
              }))
              .map(_.reverse.foldLeft[IfStmt](null)({
                case (nextIfTree, statusCodeName) =>
                  val responseSubclassType = JavaParser.parseClassOrInterfaceType(s"${responseName}.${statusCodeName}")
                  new IfStmt(
                    new InstanceOfExpr(new NameExpr("result"), responseSubclassType),
                    new BlockStmt(
                      new NodeList(
                        new ExpressionStmt(
                          new MethodCallExpr(
                            new NameExpr("builder"),
                            "entity",
                            new NodeList[Expression](
                              new MethodCallExpr(
                                new EnclosedExpr(new CastExpr(responseSubclassType, new NameExpr("result"))),
                                "getEntityBody"
                              )
                            )
                          )
                        )
                      )
                    ),
                    nextIfTree
                  )
              }))

            (
              List[Statement](
                new ExpressionStmt(
                  new VariableDeclarationExpr(
                    new VariableDeclarator(
                      RESPONSE_BUILDER_TYPE,
                      "builder",
                      new MethodCallExpr(new NameExpr("Response"),
                        "status",
                        new NodeList[Expression](new MethodCallExpr(new NameExpr("result"), "getStatusCode")))
                    ),
                    FINAL
                  )
                )
              ) ++ entitySetterIfTree ++ List(
                new ExpressionStmt(
                  new MethodCallExpr(new NameExpr("asyncResponse"), "resume", new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build")))
                )
              )
            ).toNodeList
          })({ _ =>
            new NodeList(
              new ExpressionStmt(
                new MethodCallExpr(
                  new NameExpr("asyncResponse"),
                  "resume",
                  new NodeList[Expression](new NameExpr("result"))
                )
              )
            )
          })

      val whenCompleteLambda = new LambdaExpr(
        new NodeList(
          new Parameter(util.EnumSet.of(FINAL), responseType, new SimpleName("result")),
          new Parameter(util.EnumSet.of(FINAL), THROWABLE_TYPE, new SimpleName("err"))
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
                      new NameExpr("asyncResponse"),
                      "resume",
                      new NodeList[Expression](
                        new MethodCallExpr(new MethodCallExpr(
                                             new NameExpr("Response"),
                                             "status",
                                             new NodeList[Expression](new IntegerLiteralExpr(500))
                                           ),
                                           "build")
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

      val securityHandlerArg: Option[Expression] = NonEmptyList
        .fromList(securityParams.principals)
        .fold(
          if (handlerNeedsPrincipalList) {
            Some(new MethodCallExpr(new NameExpr("Collections"), "emptyList"))
          } else {
            Option.empty
          }
        )(
          principals =>
            Some(
              new MethodCallExpr(
                new MethodCallExpr(
                  new MethodCallExpr(
                    new MethodCallExpr(
                      new MethodCallExpr(
                        new NameExpr("Arrays"),
                        "asList",
                        principals.map[Expression]({
                          case (tpe, name) if tpe.isOptional => new NameExpr(name)
                          case (_, name) => new MethodCallExpr(new NameExpr("Optional"), "of", new NodeList[Expression](new NameExpr(name)))
                        }).toList.toNodeList
                      ),
                      "stream"
                    ),
                    "filter",
                    new NodeList[Expression](new MethodReferenceExpr(new NameExpr("Optional"), null, "isPresent"))
                  ),
                  "map",
                  new NodeList[Expression](new MethodReferenceExpr(new NameExpr("Optional"), null, "get"))
                ),
                "collect",
                new NodeList[Expression](new MethodCallExpr(new NameExpr("Collectors"), "toList"))
              )
            )
        )

      val handlerCall = new MethodCallExpr(
        new FieldAccessExpr(new ThisExpr, "handler"),
        methodName,
        (securityHandlerArg.toList ++ methodParams.map(param => new NameExpr(param.getName.asString))).toNodeList
      )

      method.setBody(
        new BlockStmt(
          (
            securityParams.routeStatements :+
              new ExpressionStmt(new MethodCallExpr(handlerCall, "whenComplete", new NodeList[Expression](whenCompleteLambda)))
          ).toNodeList
        )
      )

      method
    })

    val futureResponseType = completionStageType(responseType.clone())
    val handlerMethodSig   = new MethodDeclaration(util.EnumSet.noneOf(classOf[Modifier]), futureResponseType, methodName)
    if (handlerNeedsPrincipalList) {
      handlerMethodSig.addParameter(
        new Parameter(util.EnumSet.of(FINAL), listType(PRINCIPAL_TYPE), new SimpleName("authPrincipals"))
      )
    }
    (
      parameters.pathParams ++
        parameters.headerParams ++
        parameters.queryStringParams ++
        parameters.formParams ++
        parameters.bodyParams
    ).map(_.param).foreach({ parameter =>
      handlerMethodSig.addParameter(parameter.clone())
    })
    handlerMethodSig.setBody(null)

    Target.pure((routeMethods.toList, handlerMethodSig))
  }

  def generateRoutes(tracing: Boolean,
                     resourceName: String,
                     basePath: Option[String],
                     routes: List[(String, Option[TracingField[JavaLanguage]], RouteMeta, ScalaParameters[JavaLanguage], Responses[JavaLanguage])],
                     protocolElems: List[StrictProtocolElems[JavaLanguage]],
                     securitySchemes: Map[String, SecurityScheme],
                     unknownHttpAuthSchemeHandler: UnknownHttpAuthSchemeHandler = emptyUnknownHttpAuthSchemeHandler): Target[RenderedRoutes[JavaLanguage]] =
    for {
      resourceType <- safeParseClassOrInterfaceType(resourceName)
      handlerName = s"${resourceName.replaceAll("Resource$", "")}Handler"
      handlerType <- safeParseClassOrInterfaceType(handlerName)

      basePathComponents = basePath.toList.flatMap(splitPathComponents)
      commonPathPrefix   = findPathPrefix(routes.map(_._3.path))

      processedRoutes <- routes
        .traverse({
          case (_, _, RouteMeta(path, httpMethod, operation), parameters, responses) =>
            for {
              securityParameters <- generateSecurityParams(operation, securitySchemes, unknownHttpAuthSchemeHandler)
              processedRoutes <- generateRoute(
                operation,
                path,
                commonPathPrefix,
                httpMethod,
                parameters,
                responses,
                protocolElems,
                securityParameters,
                unknownHttpAuthSchemeHandler,
                handlerName
              )
            } yield processedRoutes
        })

      (routeMethods, handlerMethodSigs) = processedRoutes.unzip
    } yield {
      val resourceConstructor = new ConstructorDeclaration(util.EnumSet.of(PUBLIC), resourceName)
      resourceConstructor.addParameter(new Parameter(util.EnumSet.of(FINAL), handlerType, new SimpleName("handler")))
      resourceConstructor.setBody(
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "handler"), new NameExpr("handler"), AssignExpr.Operator.ASSIGN))
          )
        )
      )

      val annotations = List(
        new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr((basePathComponents ++ commonPathPrefix).mkString("/", "/", "")))
      )

      val supportDefinitions = List[BodyDeclaration[_ <: BodyDeclaration[_]]](
        new FieldDeclaration(
          util.EnumSet.of(PRIVATE, STATIC, FINAL),
          new VariableDeclarator(
            LOGGER_TYPE,
            "logger",
            new MethodCallExpr(new NameExpr("LoggerFactory"), "getLogger", new NodeList[Expression](new ClassExpr(resourceType)))
          )
        ),
        new FieldDeclaration(util.EnumSet.of(PRIVATE, FINAL), new VariableDeclarator(handlerType, "handler")),
        resourceConstructor
      )

      RenderedRoutes[JavaLanguage](routeMethods.flatten, annotations, handlerMethodSigs, supportDefinitions, List.empty)
    }

  object ServerTermInterp extends (ServerTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ServerTerm[JavaLanguage, T]): Target[T] = term match {
      case GetExtraImports(tracing) =>
        List(
          "io.dropwizard.auth.Auth",
          "javax.ws.rs.Consumes",
          "javax.ws.rs.DELETE",
          "javax.ws.rs.FormParam",
          "javax.ws.rs.GET",
          "javax.ws.rs.HEAD",
          "javax.ws.rs.HeaderParam",
          "javax.ws.rs.OPTIONS",
          "javax.ws.rs.POST",
          "javax.ws.rs.PUT",
          "javax.ws.rs.Path",
          "javax.ws.rs.PathParam",
          "javax.ws.rs.Produces",
          "javax.ws.rs.QueryParam",
          "javax.ws.rs.container.AsyncResponse",
          "javax.ws.rs.container.Suspended",
          "javax.ws.rs.core.MediaType",
          "javax.ws.rs.core.Response",
          "java.util.Arrays",
          "java.util.Collections",
          "java.util.List",
          "java.util.Optional",
          "java.util.concurrent.CompletionStage",
          "java.util.stream.Collectors",
          "java.security.Principal",
          "org.glassfish.jersey.media.multipart.FormDataParam",
          "org.slf4j.Logger",
          "org.slf4j.LoggerFactory"
        ).traverse(safeParseRawImport)

      case BuildTracingFields(operation, resourceName, tracing) =>
        if (tracing) {
          Target.raiseError(s"Tracing is not yet supported by this framework")
        } else {
          Target.pure(Option.empty)
        }

      case GenerateRoutes(tracing, resourceName, basePath, routes, protocolElems, securitySchemes) =>
        generateRoutes(tracing, resourceName, basePath, routes, protocolElems, securitySchemes)

      case GetExtraRouteParams(tracing) =>
        if (tracing) {
          Target.raiseError(s"Tracing is not yet supported by this framework")
        } else {
          Target.pure(List.empty)
        }

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        for {
          abstractResponseClassName <- safeParseSimpleName(s"${operationId.toPascalCase}Response").map(_.asString)
          abstractResponseClassType <- safeParseClassOrInterfaceType(abstractResponseClassName)

          // TODO: verify valueTypes are in protocolElems

          abstractResponseClass <- generateResponseSuperClass(abstractResponseClassName)
          responseClasses       <- responses.value.traverse(resp => generateResponseClass(abstractResponseClassType, resp, None))
        } yield {
          sortDefinitions(responseClasses.flatMap({ case (cls, creator) => List[BodyDeclaration[_ <: BodyDeclaration[_]]](cls, creator) }))
            .foreach(abstractResponseClass.addMember)

          abstractResponseClass :: Nil
        }

      case GenerateSupportDefinitions(tracing, securitySchemes) =>
        for {
          annotationImports <- List(
            "java.lang.annotation.ElementType",
            "java.lang.annotation.Retention",
            "java.lang.annotation.RetentionPolicy",
            "java.lang.annotation.Target",
            "javax.ws.rs.HttpMethod"
          ).traverse(safeParseRawImport)

          shower <- SerializationHelpers.showerSupportDef

          jersey <- SerializationHelpers.guardrailJerseySupportDef

          apiKeyQueryAuthPrincipal <- DropwizardHelpers.apiKeyQueryAuthPrincipalSupportDef
          apiKeyHeaderAuthPrincipal <- DropwizardHelpers.apiKeyHeaderAuthPrincipalSupportDef
          apiKeyCookieAuthPrincipal <- DropwizardHelpers.apiKeyCookieAuthPrincipalSupportDef
          httpBasicAuthPrincipal <- DropwizardHelpers.httpBasicAuthPrincipalSupportDef
          httpBearerAuthPrincipal <- DropwizardHelpers.httpBearerAuthPrincipalSupportDef
          oauthAuthPrincipal <- DropwizardHelpers.oauthAuthPrincipalSupportDef
          openIdConnectAuthPrincipal <- DropwizardHelpers.openIdConnectAuthPrincipalSupportDef
          apiKeyAuthFilter <- DropwizardHelpers.apiKeyAuthFilterSupportDef
        } yield {
          def httpMethodAnnotation(name: String): SupportDefinition[JavaLanguage] = {
            val annotationDecl = new AnnotationDeclaration(util.EnumSet.of(PUBLIC), name)
              .addAnnotation(
                new SingleMemberAnnotationExpr(new Name("Target"),
                                               new ArrayInitializerExpr(new NodeList(new FieldAccessExpr(new NameExpr("ElementType"), "METHOD"))))
              )
              .addAnnotation(new SingleMemberAnnotationExpr(new Name("Retention"), new FieldAccessExpr(new NameExpr("RetentionPolicy"), "RUNTIME")))
              .addAnnotation(new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr(name)))
            SupportDefinition[JavaLanguage](new Name(name), annotationImports, annotationDecl)
          }

          val authFilters = securitySchemes.collect({
            case (schemeName, scheme: ApiKeySecurityScheme) => (schemeName, scheme)
          }).map({
            case (schemeName, ApiKeySecurityScheme(name, in, _, _)) =>
              val className = s"${schemeName.toPascalCase}ApiKeyAuthFilter"
              val classType = JavaParser.parseClassOrInterfaceType(className).setTypeArguments(GENERIC_A_TYPE)
              val classTypeDiamonded = classType.clone().setTypeArguments(new NodeList[Type])
              val principalName = s"ApiKey${in.toString.toLowerCase(Locale.US).capitalize}AuthPrincipal"
              val principalType = JavaParser.parseClassOrInterfaceType(principalName).setTypeArguments(GENERIC_A_TYPE)

              val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, className)
                .setTypeParameters(new NodeList(new TypeParameter("A")))
                .setExtendedTypes(new NodeList(apiKeyAuthFilterType(principalType)))
              cls.addConstructor(PRIVATE)
                .setBody(new BlockStmt(new NodeList(
                  new ExpressionStmt(new MethodCallExpr(
                    "super",
                    new StringLiteralExpr(name),
                    new FieldAccessExpr(new NameExpr("In"), in.toString.toUpperCase(Locale.US))
                  ))
                )))

              val builderCls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, "Builder")
                .setTypeParameters(new NodeList(new TypeParameter("A")))
                .setExtendedTypes(new NodeList(apiKeyAuthFilterBuilderType(principalType, classType)))
              builderCls.addMethod("newInstance", PUBLIC)
                .setType(classType)
                .setBody(new BlockStmt(new NodeList(
                  new ReturnStmt(new ObjectCreationExpr(null, classTypeDiamonded, new NodeList))
                )))
              cls.addMember(builderCls)

              SupportDefinition[JavaLanguage](new Name(className), List.empty, cls)
          })

          val authPrincipalDefs = List(
            securitySchemes.collectFirst({ case (_, ApiKeySecurityScheme(_, SwSecurityScheme.In.QUERY, _, _)) => apiKeyQueryAuthPrincipal }),
            securitySchemes.collectFirst({ case (_, ApiKeySecurityScheme(_, SwSecurityScheme.In.HEADER, _, _)) => apiKeyHeaderAuthPrincipal }),
            securitySchemes.collectFirst({ case (_, ApiKeySecurityScheme(_, SwSecurityScheme.In.COOKIE, _, _)) => apiKeyCookieAuthPrincipal }),
            securitySchemes.collectFirst({ case (_, HttpSecurityScheme("basic", _, _)) => httpBasicAuthPrincipal }),
            securitySchemes.collectFirst({ case (_, HttpSecurityScheme("bearer", _, _)) => httpBearerAuthPrincipal }),
            securitySchemes.collectFirst({ case (_, _: OAuth2SecurityScheme) => oauthAuthPrincipal }),
            securitySchemes.collectFirst({ case (_, _: OpenIdConnectSecurityScheme) => openIdConnectAuthPrincipal })
          ).flatten

          List(
            shower,
            jersey,
            httpMethodAnnotation("PATCH"),
            httpMethodAnnotation("TRACE")
          ) ++ authPrincipalDefs ++ (if (authFilters.nonEmpty) List(apiKeyAuthFilter) else List.empty) ++ authFilters
        }

      case RenderClass(className, handlerName, classAnnotations, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        safeParseSimpleName(className) >>
          safeParseSimpleName(handlerName) >>
          Target.pure(doRenderClass(className, classAnnotations, supportDefinitions, combinedRouteTerms) :: Nil)

      case RenderHandler(handlerName, methodSigs, handlerDefinitions, responseDefinitions) =>
        val handlerClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), true, handlerName)
        sortDefinitions(methodSigs ++ responseDefinitions).foreach(handlerClass.addMember)
        Target.pure(handlerClass)
    }

    // Lift this function out of RenderClass above to work around a 2.11.x compiler syntax bug
    private def doRenderClass(className: String,
                              classAnnotations: List[AnnotationExpr],
                              supportDefinitions: List[BodyDeclaration[_ <: BodyDeclaration[_]]],
                              combinedRouteTerms: List[Node]): ClassOrInterfaceDeclaration = {
      val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, className)
      classAnnotations.foreach(cls.addAnnotation)
      sortDefinitions(supportDefinitions ++ combinedRouteTerms.collect({ case bd: BodyDeclaration[_] => bd }))
        .foreach(cls.addMember)
      cls
    }
  }
}
