package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.~>
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.{ Modifier, NodeList }
import com.github.javaparser.ast.`type`.{ PrimitiveType, Type, VoidType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.generators.{ Response, ScalaParameter, ScalaParameters }
import com.twilio.guardrail.{ ADT, ClassDefinition, EnumDefinition, RandomType, RenderedRoutes, StrictProtocolElems, SupportDefinition, Target }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.shims.OperationExt
import com.twilio.guardrail.terms.RouteMeta
import io.swagger.v3.oas.models.PathItem.HttpMethod
import io.swagger.v3.oas.models.responses.ApiResponse
import java.util
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
  private val RESPONSE_BUILDER_TYPE = JavaParser.parseClassOrInterfaceType("Response.ResponseBuilder")
  private val LOGGER_TYPE           = JavaParser.parseClassOrInterfaceType("Logger")

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

  object ServerTermInterp extends (ServerTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ServerTerm[JavaLanguage, T]): Target[T] = term match {
      case GetExtraImports(tracing) =>
        List(
          "javax.ws.rs.Consumes",
          "javax.ws.rs.DELETE",
          "javax.ws.rs.FormParam",
          "javax.ws.rs.GET",
          "javax.ws.rs.HEAD",
          "javax.ws.rs.HeaderParam",
          "javax.ws.rs.HttpMethod",
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

      case GenerateRoutes(tracing, resourceName, basePath, routes, protocolElems) =>
        for {
          resourceType <- safeParseClassOrInterfaceType(resourceName)
          handlerName = s"${resourceName.replaceAll("Resource$", "")}Handler"
          handlerType <- safeParseClassOrInterfaceType(handlerName)
        } yield {
          val basePathComponents = basePath.toList.flatMap(splitPathComponents)
          val commonPathPrefix   = findPathPrefix(routes.map(_._3.path))

          val (routeMethods, handlerMethodSigs) = routes
            .map({
              case (operationId, tracingFields, sr @ RouteMeta(path, httpMethod, operation), parameters, responses) =>
                parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))

                val method = new MethodDeclaration(util.EnumSet.of(PUBLIC), new VoidType, operationId)
                val httpMethodAnnotation = httpMethod match {
                  case HttpMethod.DELETE  => new MarkerAnnotationExpr("DELETE")
                  case HttpMethod.GET     => new MarkerAnnotationExpr("GET")
                  case HttpMethod.HEAD    => new MarkerAnnotationExpr("HEAD")
                  case HttpMethod.OPTIONS => new MarkerAnnotationExpr("OPTIONS")
                  case HttpMethod.PATCH   => new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr("PATCH"))
                  case HttpMethod.POST    => new MarkerAnnotationExpr("POST")
                  case HttpMethod.PUT     => new MarkerAnnotationExpr("PUT")
                  case HttpMethod.TRACE   => new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr("TRACE"))
                }
                method.addAnnotation(httpMethodAnnotation)

                val pathSuffix = splitPathComponents(path).drop(commonPathPrefix.length).mkString("/", "/", "")
                if (pathSuffix.nonEmpty && pathSuffix != "/") {
                  method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr(pathSuffix)))
                }

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
                consumes
                  .map(c => new SingleMemberAnnotationExpr(new Name("Consumes"), new FieldAccessExpr(new NameExpr("MediaType"), c.toJaxRsAnnotationName)))
                  .foreach(method.addAnnotation)

                val successResponses =
                  operation.getResponses.entrySet.asScala.filter(entry => Try(entry.getKey.toInt / 100 == 2).getOrElse(false)).map(_.getValue).toList
                val produces = getBestProduces(operation.produces.flatMap(RouteMeta.ContentType.unapply).toList, successResponses, protocolElems)
                produces
                  .map(p => new SingleMemberAnnotationExpr(new Name("Produces"), new FieldAccessExpr(new NameExpr("MediaType"), p.toJaxRsAnnotationName)))
                  .foreach(method.addAnnotation)

                def addParamAnnotation(template: Parameter, annotationName: String, argName: String): Parameter = {
                  val parameter = template.clone()
                  parameter.addAnnotation(new SingleMemberAnnotationExpr(new Name(annotationName), new StringLiteralExpr(argName)))
                  parameter
                }

                val methodParams: List[Parameter] = List(
                  (parameters.pathParams, "PathParam"),
                  (parameters.headerParams, "HeaderParam"),
                  (parameters.queryStringParams, "QueryParam"),
                  (parameters.formParams, if (parameters.formParams.exists(_.isFile)) "FormDataParam" else "FormParam")
                ).flatMap({
                  case (params, annotationName) =>
                    params.map(param => addParamAnnotation(param.param, annotationName, param.argName.value))
                }) ++ parameters.bodyParams.map(_.param)

                methodParams.foreach(method.addParameter)
                method.addParameter(
                  new Parameter(util.EnumSet.of(FINAL), ASYNC_RESPONSE_TYPE, new SimpleName("asyncResponse")).addMarkerAnnotation("Suspended")
                )

                val responseName = s"${operationId.capitalize}Response"
                val responseType = JavaParser.parseClassOrInterfaceType(responseName)

                val responseBodyIfBranches = responses.value
                  .collect({
                    case r @ Response(_, Some(_)) => r
                  })
                  .map({
                    case Response(statusCodeName, valueType) =>
                      val responseType = JavaParser.parseClassOrInterfaceType(s"${responseName}.${statusCodeName}")
                      new IfStmt(
                        new InstanceOfExpr(new NameExpr("result"), responseType),
                        new BlockStmt(
                          new NodeList(
                            new ExpressionStmt(
                              new MethodCallExpr(
                                new NameExpr("builder"),
                                "entity",
                                new NodeList[Expression](
                                  new MethodCallExpr(new EnclosedExpr(new CastExpr(responseType, new NameExpr("result"))), "getValue")
                                )
                              )
                            )
                          )
                        ),
                        null
                      )
                  })
                NonEmptyList
                  .fromList(responseBodyIfBranches)
                  .foreach(_.reduceLeft({ (prev, next) =>
                    prev.setElseStmt(next)
                    next
                  }))

                val whenCompleteLambda = new LambdaExpr(
                  new NodeList(
                    new Parameter(util.EnumSet.of(FINAL), JavaParser.parseClassOrInterfaceType(responseName), new SimpleName("result")),
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
                        new BlockStmt(
                          new NodeList(
                            List(
                              Option(
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
                              ),
                              responseBodyIfBranches.headOption,
                              Option(
                                new ExpressionStmt(
                                  new MethodCallExpr(new NameExpr("asyncResponse"),
                                                     "resume",
                                                     new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build")))
                                )
                              )
                            ).flatten: _*
                          )
                        )
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

                val futureResponseType = completionStageType(responseType)
                val handlerMethodSig   = new MethodDeclaration(util.EnumSet.noneOf(classOf[Modifier]), futureResponseType, operationId)
                (parameters.pathParams ++ parameters.headerParams ++ parameters.queryStringParams ++ parameters.formParams ++ parameters.bodyParams).foreach({
                  parameter =>
                    handlerMethodSig.addParameter(parameter.param.clone())
                })
                handlerMethodSig.setBody(null)

                (method, handlerMethodSig)
            })
            .unzip

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

          val supportDefinitions = List[BodyDeclaration[_]](
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
        } yield {
          val abstractResponseClass = {
            val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, ABSTRACT), false, abstractResponseClassName)
            cls.addField(PrimitiveType.intType, "statusCode", PRIVATE, FINAL)

            val clsConstructor = cls.addConstructor(PROTECTED)
            clsConstructor.addParameter(new Parameter(util.EnumSet.of(FINAL), PrimitiveType.intType, new SimpleName("statusCode")))
            clsConstructor.setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "statusCode"), new NameExpr("statusCode"), AssignExpr.Operator.ASSIGN))
                )
              )
            )

            val getStatusCodeMethod = cls.addMethod("getStatusCode", PUBLIC)
            getStatusCodeMethod.setType(PrimitiveType.intType)
            getStatusCodeMethod.setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(new FieldAccessExpr(new ThisExpr, "statusCode"))
                )
              )
            )

            cls
          }

          val responseClasses = responses.value.map { response =>
            val clsName: String = response.statusCodeName.asString
            val clsType         = JavaParser.parseClassOrInterfaceType(clsName)
            val cls             = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, clsName)
            cls.setExtendedTypes(new NodeList(abstractResponseClassType))

            val (fields, constructor, creator, methods) =
              response.value.fold[(List[FieldDeclaration], ConstructorDeclaration, BodyDeclaration[_], List[MethodDeclaration])]({
                val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
                constructor.setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(new MethodCallExpr("super", new IntegerLiteralExpr(response.statusCode)))
                    )
                  )
                )

                val creator = new FieldDeclaration(
                  util.EnumSet.of(PUBLIC, STATIC, FINAL),
                  new VariableDeclarator(clsType, clsName, new ObjectCreationExpr(null, clsType, new NodeList))
                )

                (List.empty[FieldDeclaration], constructor, creator, List.empty[MethodDeclaration])
              })({
                case (valueType, _) =>
                  val unboxedValueType: Type = valueType.unbox
                  val valueField             = new FieldDeclaration(util.EnumSet.of(PRIVATE, FINAL), new VariableDeclarator(unboxedValueType, "value"))

                  val constructParam = new Parameter(util.EnumSet.of(FINAL), unboxedValueType, new SimpleName("value"))

                  val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
                    .addParameter(constructParam)
                    .setBody(
                      new BlockStmt(
                        new NodeList(
                          new ExpressionStmt(new MethodCallExpr("super", new IntegerLiteralExpr(response.statusCode))),
                          new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "value"), new NameExpr("value"), AssignExpr.Operator.ASSIGN))
                        )
                      )
                    )

                  val creator = new MethodDeclaration(util.EnumSet.of(PUBLIC, STATIC), clsType, clsName)
                    .addParameter(constructParam)
                    .setBody(
                      new BlockStmt(
                        new NodeList(
                          new ReturnStmt(new ObjectCreationExpr(null, clsType, new NodeList(new NameExpr("value"))))
                        )
                      )
                    )

                  val getValueMethod = new MethodDeclaration(util.EnumSet.of(PUBLIC), unboxedValueType, "getValue")
                  getValueMethod.setBody(
                    new BlockStmt(
                      new NodeList(
                        new ReturnStmt(new FieldAccessExpr(new ThisExpr, "value"))
                      )
                    )
                  )

                  (valueField :: Nil, constructor, creator, getValueMethod :: Nil)
              })

            (fields ++ Option(constructor) ++ methods).foreach(cls.addMember)
            abstractResponseClass.addMember(creator)

            cls
          }
          responseClasses.foreach(abstractResponseClass.addMember)

          abstractResponseClass :: Nil
        }

      case GenerateSupportDefinitions(tracing) =>
        val showerClass = SHOWER_CLASS_DEF
        Target.pure(
          List(
            SupportDefinition[JavaLanguage](new Name(showerClass.getNameAsString), List.empty, showerClass)
          )
        )

      case RenderClass(className, handlerName, classAnnotations, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        def doRender: Target[List[BodyDeclaration[_]]] = {
          val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, className)
          classAnnotations.foreach(cls.addAnnotation)
          supportDefinitions.foreach(cls.addMember)
          combinedRouteTerms.foreach({
            case bd: BodyDeclaration[_] => cls.addMember(bd)
            case _                      =>
          })

          Target.pure(cls +: responseDefinitions)
        }

        safeParseSimpleName(className) >>
          safeParseSimpleName(handlerName) >>
          doRender

      case RenderHandler(handlerName, methodSigs, handlerDefinitions) =>
        val handlerClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), true, handlerName)
        methodSigs.foreach(handlerClass.addMember)
        Target.pure(handlerClass)
    }
  }

}
