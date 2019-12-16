package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.~>
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.Modifier.Keyword._
import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, VoidType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.{ ADT, ClassDefinition, EnumDefinition, RandomType, RenderedRoutes, StrictProtocolElems, SupportDefinition, Target }
import com.twilio.guardrail.extract.ServerRawResponse
import com.twilio.guardrail.generators.{ ScalaParameter, ScalaParameters }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.Response
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.shims.OperationExt
import com.twilio.guardrail.terms.RouteMeta
import io.swagger.v3.oas.models.responses.ApiResponse

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
      case RouteMeta.OctetStream        => "APPLICATION_OCTET_STREAM"
    }
  }

  private val ASYNC_RESPONSE_TYPE   = StaticJavaParser.parseClassOrInterfaceType("AsyncResponse")
  private val RESPONSE_TYPE         = StaticJavaParser.parseClassOrInterfaceType("Response")
  private val RESPONSE_BUILDER_TYPE = StaticJavaParser.parseClassOrInterfaceType("Response.ResponseBuilder")
  private val LOGGER_TYPE           = StaticJavaParser.parseClassOrInterfaceType("Logger")

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

  private def getBestProduces(
      contentTypes: List[RouteMeta.ContentType],
      responses: List[ApiResponse],
      protocolElems: List[StrictProtocolElems[JavaLanguage]]
  ): Option[RouteMeta.ContentType] = {
    val priorityOrder = NonEmptyList.of(
      RouteMeta.ApplicationJson,
      RouteMeta.TextPlain,
      RouteMeta.OctetStream
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
        .map(_._1)
        .orElse({
          if (response.statusCode >= 400 && response.statusCode <= 599) {
            errorEntityFallbackType
          } else {
            None
          }
        })
        .fold[(List[BodyDeclaration[_ <: BodyDeclaration[_]]], BodyDeclaration[_ <: BodyDeclaration[_]])]({
          val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
          val _ = constructor.setBody(
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
            new NodeList(publicModifier, staticModifier, finalModifier),
            new VariableDeclarator(clsType, clsName, new ObjectCreationExpr(null, clsType, new NodeList))
          )

          (List(constructor), creator)
        })({ valueType =>
          val constructParam = new Parameter(new NodeList(finalModifier), valueType.unbox, new SimpleName("entityBody"))

          val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
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
        })

      sortDefinitions(classDecls).foreach(cls.addMember)
      (cls, creator)
    }
  }

  object ServerTermInterp extends (ServerTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ServerTerm[JavaLanguage, T]): Target[T] = term match {
      case GetExtraImports(tracing) =>
        List(
          "javax.inject.Inject",
          "javax.validation.constraints.NotNull",
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
          "java.util.Optional",
          "java.util.concurrent.CompletionStage",
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
        for {
          resourceType <- safeParseClassOrInterfaceType(resourceName)
          handlerName = s"${resourceName.replaceAll("Resource$", "")}Handler"
          handlerType <- safeParseClassOrInterfaceType(handlerName)
        } yield {
          val basePathComponents = basePath.toList.flatMap(splitPathComponents)
          val commonPathPrefix   = findPathPrefix(routes.map(_._3.path.get))

          val (routeMethods, handlerMethodSigs) = routes
            .map({
              case (operationId, tracingFields, sr @ RouteMeta(path, httpMethod, operation, securityRequirements), parameters, responses) =>
                parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))

                val method = new MethodDeclaration(new NodeList(publicModifier), new VoidType, operationId)
                  .addAnnotation(new MarkerAnnotationExpr(httpMethod.toString))

                val pathSuffix = splitPathComponents(path.unwrapTracker).drop(commonPathPrefix.length).mkString("/", "/", "")
                if (pathSuffix.nonEmpty && pathSuffix != "/") {
                  method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr(pathSuffix)))
                }

                val consumes = getBestConsumes(operation.get.consumes.flatMap(RouteMeta.ContentType.unapply).toList, parameters)
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
                consumes
                  .map(c => new SingleMemberAnnotationExpr(new Name("Consumes"), new FieldAccessExpr(new NameExpr("MediaType"), c.toJaxRsAnnotationName)))
                  .foreach(method.addAnnotation)

                val successResponses =
                  operation.get.getResponses.entrySet.asScala.filter(entry => Try(entry.getKey.toInt / 100 == 2).getOrElse(false)).map(_.getValue).toList
                val produces = getBestProduces(operation.get.produces.flatMap(RouteMeta.ContentType.unapply).toList, successResponses, protocolElems)
                produces
                  .map(p => new SingleMemberAnnotationExpr(new Name("Produces"), new FieldAccessExpr(new NameExpr("MediaType"), p.toJaxRsAnnotationName)))
                  .foreach(method.addAnnotation)

                def addValidationAnnotations(parameter: Parameter, param: ScalaParameter[JavaLanguage]): Parameter = {
                  if (param.required) {
                    // NB: The order here is actually critical.  In the case where we're using multipart,
                    // the @NotNull annotation *must* come before the @FormDataParam annotation.  See:
                    // https://github.com/eclipse-ee4j/jersey/issues/3632
                    parameter.getAnnotations.add(0, new MarkerAnnotationExpr("NotNull"))
                  }
                  parameter
                }

                def addParamAnnotation(parameter: Parameter, param: ScalaParameter[JavaLanguage], annotationName: String): Parameter =
                  parameter.addAnnotation(new SingleMemberAnnotationExpr(new Name(annotationName), new StringLiteralExpr(param.argName.value)))

                def boxParameterTypes(parameter: Parameter): Parameter = {
                  if (parameter.getType.isPrimitiveType) {
                    parameter.setType(parameter.getType.asPrimitiveType.toBoxedType)
                  }
                  parameter
                }

                val annotatedMethodParams: List[Parameter] = List(
                  (parameters.pathParams, "PathParam"),
                  (parameters.headerParams, "HeaderParam"),
                  (parameters.queryStringParams, "QueryParam"),
                  (parameters.formParams, if (consumes.contains(RouteMeta.MultipartFormData)) "FormDataParam" else "FormParam")
                ).flatMap({
                  case (params, annotationName) =>
                    params.map({ param =>
                      val parameter = param.param.clone()
                      val annotated = addParamAnnotation(parameter, param, annotationName)
                      addValidationAnnotations(annotated, param)
                    })
                })

                val bareMethodParams: List[Parameter] = parameters.bodyParams.toList
                  .map(param => addValidationAnnotations(param.param.clone(), param))

                val methodParams = (annotatedMethodParams ++ bareMethodParams).map(boxParameterTypes)

                methodParams.foreach(method.addParameter)
                method.addParameter(
                  new Parameter(new NodeList(finalModifier), ASYNC_RESPONSE_TYPE, new SimpleName("asyncResponse")).addMarkerAnnotation("Suspended")
                )

                val (responseName, responseType, resultResumeBody) =
                  ServerRawResponse(operation)
                    .filter(_ == true)
                    .fold({
                      val responseName = s"${handlerName}.${operationId.capitalize}Response"

                      val entitySetterIfTree = NonEmptyList
                        .fromList(responses.value.collect({
                          case Response(statusCodeName, Some(_), _) => statusCodeName
                        }))
                        .map(_.reverse.foldLeft[IfStmt](null)({
                          case (nextIfTree, statusCodeName) =>
                            val responseSubclassType = StaticJavaParser.parseClassOrInterfaceType(s"${responseName}.${statusCodeName}")
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
                        responseName,
                        StaticJavaParser.parseClassOrInterfaceType(responseName),
                        (
                          List[Statement](
                            new ExpressionStmt(
                              new VariableDeclarationExpr(
                                new VariableDeclarator(
                                  RESPONSE_BUILDER_TYPE,
                                  "builder",
                                  new MethodCallExpr(
                                    new NameExpr("Response"),
                                    "status",
                                    new NodeList[Expression](new MethodCallExpr(new NameExpr("result"), "getStatusCode"))
                                  )
                                ),
                                finalModifier
                              )
                            )
                          ) ++ entitySetterIfTree ++ List(
                                new ExpressionStmt(
                                  new MethodCallExpr(
                                    new NameExpr("asyncResponse"),
                                    "resume",
                                    new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build"))
                                  )
                                )
                              )
                        ).toNodeList
                      )
                    })({ _ =>
                      (
                        "Response",
                        RESPONSE_TYPE,
                        new NodeList(
                          new ExpressionStmt(
                            new MethodCallExpr(
                              new NameExpr("asyncResponse"),
                              "resume",
                              new NodeList[Expression](new NameExpr("result"))
                            )
                          )
                        )
                      )
                    })

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
                                  new StringLiteralExpr(s"${handlerName}.${operationId} threw an exception ({}): {}"),
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
                                  new MethodCallExpr(
                                    new MethodCallExpr(
                                      new NameExpr("Response"),
                                      "status",
                                      new NodeList[Expression](new IntegerLiteralExpr(500))
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

                val handlerCall = new MethodCallExpr(
                  new FieldAccessExpr(new ThisExpr, "handler"),
                  operationId,
                  new NodeList[Expression](methodParams.map(param => new NameExpr(param.getName.asString)): _*)
                )

                method.setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(new MethodCallExpr(handlerCall, "whenComplete", new NodeList[Expression](whenCompleteLambda)))
                    )
                  )
                )

                val futureResponseType = completionStageType(responseType.clone())
                val handlerMethodSig   = new MethodDeclaration(new NodeList(), futureResponseType, operationId)
                (parameters.pathParams ++ parameters.headerParams ++ parameters.queryStringParams ++ parameters.formParams ++ parameters.bodyParams).foreach({
                  parameter =>
                    handlerMethodSig.addParameter(parameter.param.clone())
                })
                handlerMethodSig.setBody(null)

                (method, handlerMethodSig)
            })
            .unzip

          val resourceConstructor = new ConstructorDeclaration(new NodeList(publicModifier), resourceName)
          resourceConstructor.addAnnotation(new MarkerAnnotationExpr(new Name("Inject")))
          resourceConstructor.addParameter(new Parameter(new NodeList(finalModifier), handlerType, new SimpleName("handler")))
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

          RenderedRoutes[JavaLanguage](routeMethods, annotations, handlerMethodSigs, supportDefinitions, List.empty)
        }

      case GetExtraRouteParams(tracing) =>
        if (tracing) {
          Target.raiseError(s"Tracing is not yet supported by this framework")
        } else {
          Target.pure(List.empty)
        }

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        for {
          abstractResponseClassName <- safeParseSimpleName(s"${operationId.capitalize}Response").map(_.asString)
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
        } yield {
          def httpMethodAnnotation(name: String): SupportDefinition[JavaLanguage] = {
            val annotationDecl = new AnnotationDeclaration(new NodeList(publicModifier), name)
              .addAnnotation(
                new SingleMemberAnnotationExpr(
                  new Name("Target"),
                  new ArrayInitializerExpr(new NodeList(new FieldAccessExpr(new NameExpr("ElementType"), "METHOD")))
                )
              )
              .addAnnotation(new SingleMemberAnnotationExpr(new Name("Retention"), new FieldAccessExpr(new NameExpr("RetentionPolicy"), "RUNTIME")))
              .addAnnotation(new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr(name)))
            SupportDefinition[JavaLanguage](new Name(name), annotationImports, annotationDecl)
          }

          List(
            shower,
            jersey,
            httpMethodAnnotation("PATCH"),
            httpMethodAnnotation("TRACE")
          )
        }

      case RenderClass(className, handlerName, classAnnotations, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        safeParseSimpleName(className) >>
            safeParseSimpleName(handlerName) >>
            Target.pure(doRenderClass(className, classAnnotations, supportDefinitions, combinedRouteTerms) :: Nil)

      case RenderHandler(handlerName, methodSigs, handlerDefinitions, responseDefinitions) =>
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
      sortDefinitions(supportDefinitions ++ combinedRouteTerms.collect({ case bd: BodyDeclaration[_] => bd }))
        .foreach(cls.addMember)
      cls
    }
  }
}
