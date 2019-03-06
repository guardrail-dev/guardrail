package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.~>
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.{Modifier, NodeList}
import com.github.javaparser.ast.`type`.{PrimitiveType, Type, VoidType}
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.generators.Response
import com.twilio.guardrail.{RenderedRoutes, Target}
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.terms.RouteMeta
import io.swagger.v3.oas.models.PathItem.HttpMethod
import java.util

object DropwizardServerGenerator {
  private val ASYNC_RESPONSE_TYPE = JavaParser.parseClassOrInterfaceType("AsyncResponse")
  private val RESPONSE_BUILDER_TYPE = JavaParser.parseClassOrInterfaceType("Response.ResponseBuilder")
  private val LOGGER_TYPE = JavaParser.parseClassOrInterfaceType("Logger")

  private def removeEmpty(s: String): Option[String] = if (s.trim.isEmpty) None else Some(s.trim)
  private def splitPathComponents(s: String): List[String] = s.split("/").flatMap(removeEmpty).toList

  private def findPathPrefix(routePaths: List[String]): List[String] = {
    def getHeads(sss: List[List[String]]): (List[Option[String]], List[List[String]]) =
      (sss.map(_.headOption), sss.map(ss => ss.headOption.fold(List.empty[String])(_ => ss.tail)))

    def checkMatch(matching: List[String], headsToCheck: List[Option[String]], restOfHeads: List[List[String]]): List[String] = {
      headsToCheck.headOption.flatMap({ firstO => firstO.map({ first =>
        if (headsToCheck.tail.forall(_.contains(first))) {
          val (nextHeads, nextRest) = getHeads(restOfHeads)
          checkMatch(matching :+ first, nextHeads, nextRest)
        } else {
          matching
        }
      })}).getOrElse(matching)
    }

    val splitRoutePaths = routePaths.map(splitPathComponents)
    val (initialHeads, initialRest) = getHeads(splitRoutePaths)
    checkMatch(List.empty, initialHeads, initialRest)
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
          "org.slf4j.Logger",
          "org.slf4j.LoggerFactory"
        ).map(safeParseRawImport).sequence

      case BuildTracingFields(operation, resourceName, tracing) =>
        if (tracing) {
          Target.raiseError(s"Tracing is not yet supported by this framework")
        } else {
          Target.pure(Option.empty)
        }

      case GenerateRoutes(resourceName, basePath, routes, protocolElems) =>
        for {
          resourceType <- safeParseClassOrInterfaceType(resourceName)
          handlerName = s"${resourceName.replaceAll("Resource$", "")}Handler"
          handlerType <- safeParseClassOrInterfaceType(handlerName)
        } yield {
          val basePathComponents = basePath.toList.flatMap(splitPathComponents)
          val commonPathPrefix = findPathPrefix(routes.map(_._3.path))

          val (routeMethods, handlerMethodSigs) = routes.map({ case (operationId, tracingFields, sr @ RouteMeta(path, httpMethod, operation), parameters, responses) =>
            val method = new MethodDeclaration(util.EnumSet.of(PUBLIC), new VoidType, operationId)
            val httpMethodAnnotation = httpMethod match {
              case HttpMethod.DELETE => new MarkerAnnotationExpr("DELETE")
              case HttpMethod.GET => new MarkerAnnotationExpr("GET")
              case HttpMethod.HEAD => new MarkerAnnotationExpr("HEAD")
              case HttpMethod.OPTIONS => new MarkerAnnotationExpr("OPTIONS")
              case HttpMethod.PATCH => new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr("PATCH"))
              case HttpMethod.POST => new MarkerAnnotationExpr("POST")
              case HttpMethod.PUT => new MarkerAnnotationExpr("PUT")
              case HttpMethod.TRACE => new SingleMemberAnnotationExpr(new Name("HttpMethod"), new StringLiteralExpr("TRACE"))
            }
            method.addAnnotation(httpMethodAnnotation)

            val pathSuffix = splitPathComponents(path).drop(commonPathPrefix.length).mkString("/", "/", "")
            method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr(pathSuffix)))
            parameters.formParams.headOption.map(_ => "APPLICATION_FORM_URLENCODED")
              .orElse(parameters.bodyParams.map(_ => "APPLICATION_JSON"))
              .foreach(produces =>
                method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Consumes"), new FieldAccessExpr(new NameExpr("MediaType"), produces)))
              )
            if (responses.value.exists(_.value.isDefined)) {
              method.addAnnotation(new SingleMemberAnnotationExpr(new Name("Produces"), new FieldAccessExpr(new NameExpr("MediaType"), "APPLICATION_JSON")))
            }

            def addParamAnnotation(template: Parameter, annotationName: String, paramName: Name): Parameter = {
              val parameter = template.clone()
              parameter.addAnnotation(new SingleMemberAnnotationExpr(new Name(annotationName), new StringLiteralExpr(paramName.asString)))
              parameter
            }

            val methodParams: List[Parameter] = List(
              (parameters.pathParams, "PathParam"),
              (parameters.headerParams, "HeaderParam"),
              (parameters.queryStringParams, "QueryParam"),
              (parameters.formParams, "FormParam")
            ).flatMap({ case (params, annotationName) =>
              params.map(param => addParamAnnotation(param.param, annotationName, param.paramName))
            }) ++ parameters.bodyParams.map(_.param)

            methodParams.foreach(method.addParameter)
            method.addParameter(
              new Parameter(util.EnumSet.of(FINAL), ASYNC_RESPONSE_TYPE, new SimpleName("asyncResponse")).addMarkerAnnotation("Suspended")
            )

            val responseName = s"${operationId.capitalize}Response"
            val responseType = JavaParser.parseClassOrInterfaceType(responseName)

            val responseBodyIfBranches = responses.value.collect({
              case r @ Response(_, Some(_)) => r
            }).map({ case Response(statusCodeName, valueType) =>
              val responseType = JavaParser.parseClassOrInterfaceType(s"${responseName}.${statusCodeName}")
              new IfStmt(
                new InstanceOfExpr(new NameExpr("result"), responseType),
                new BlockStmt(new NodeList(
                  new ExpressionStmt(new VariableDeclarationExpr(new VariableDeclarator(responseType, s"result${statusCodeName}", new CastExpr(responseType, new NameExpr("result"))), FINAL)),
                  new ExpressionStmt(
                  new MethodCallExpr(new NameExpr("builder"), "entity", new NodeList[Expression](
                    new MethodCallExpr(new NameExpr(s"result${statusCodeName}"), "getValue")
                  ))
                ))),
                null
              )
            })
            NonEmptyList.fromList(responseBodyIfBranches).foreach(_.reduceLeft({ (prev, next) =>
              prev.setElseStmt(next)
              next
            }))

            val whenCompleteLambda = new LambdaExpr(
              new NodeList(
                new Parameter(util.EnumSet.of(FINAL), JavaParser.parseClassOrInterfaceType(responseName), new SimpleName("result")),
                new Parameter(util.EnumSet.of(FINAL), THROWABLE_TYPE, new SimpleName("err"))
              ),
              new BlockStmt(new NodeList(new IfStmt(
                new BinaryExpr(new NameExpr("err"), new NullLiteralExpr, BinaryExpr.Operator.NOT_EQUALS),
                new BlockStmt(new NodeList(
                  new ExpressionStmt(new MethodCallExpr(new NameExpr("logger"), "error", new NodeList[Expression](
                    new StringLiteralExpr(s"${handlerName}.${operationId} threw an exception ({}): {}"),
                    new MethodCallExpr(new MethodCallExpr(new NameExpr("err"), "getClass"), "getName"),
                    new MethodCallExpr(new NameExpr("err"), "getMessage"),
                    new NameExpr("err")
                  ))),
                  new ExpressionStmt(new MethodCallExpr(new NameExpr("asyncResponse"), "resume", new NodeList[Expression](
                    new MethodCallExpr(new MethodCallExpr(
                      new NameExpr("Response"),
                      "status",
                      new NodeList[Expression](new IntegerLiteralExpr(500))
                    ), "build")
                  )))
                )),
                new BlockStmt(new NodeList(List(
                  Option(new ExpressionStmt(new VariableDeclarationExpr(
                    new VariableDeclarator(
                      RESPONSE_BUILDER_TYPE,
                      "builder",
                      new MethodCallExpr(new NameExpr("Response"), "status", new NodeList[Expression](new MethodCallExpr(new NameExpr("result"), "getStatusCode")))
                    ),
                    FINAL
                  ))),
                  responseBodyIfBranches.headOption,
                  Option(new ExpressionStmt(new MethodCallExpr(new NameExpr("asyncResponse"), "resume", new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build")))))
                ).flatten: _*))
              ))),
              true
            )

            val handlerCall = new MethodCallExpr(
              new FieldAccessExpr(new ThisExpr, "handler"), operationId,
              new NodeList[Expression](methodParams.map(param => new NameExpr(param.getName.asString)): _*)
            )

            method.setBody(new BlockStmt(new NodeList(
              new ExpressionStmt(new MethodCallExpr(handlerCall, "whenComplete", new NodeList[Expression](whenCompleteLambda)))
            )))

            val futureResponseType = completionStageType.setTypeArguments(responseType)
            val handlerMethodSig = new MethodDeclaration(util.EnumSet.noneOf(classOf[Modifier]), futureResponseType, operationId)
            (parameters.pathParams ++ parameters.headerParams ++ parameters.queryStringParams ++ parameters.formParams ++ parameters.bodyParams).foreach({ parameter =>
              handlerMethodSig.addParameter(parameter.param.clone())
            })
            handlerMethodSig.setBody(null)

            (method, handlerMethodSig)
          }).unzip

          val resourceConstructor = new ConstructorDeclaration(util.EnumSet.of(PUBLIC), resourceName)
          resourceConstructor.addParameter(new Parameter(util.EnumSet.of(FINAL), handlerType, new SimpleName("handler")))
          resourceConstructor.setBody(new BlockStmt(new NodeList(
            new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "handler"), new NameExpr("handler"), AssignExpr.Operator.ASSIGN))
          )))

          val annotations = List(
            new SingleMemberAnnotationExpr(new Name("Path"), new StringLiteralExpr((basePathComponents ++ commonPathPrefix).mkString("/", "/", "")))
          )

          val supportDefinitions = List[BodyDeclaration[_]](
            new FieldDeclaration(util.EnumSet.of(PRIVATE, STATIC, FINAL), new VariableDeclarator(
              LOGGER_TYPE, "logger",
              new MethodCallExpr(new NameExpr("LoggerFactory"), "getLogger", new NodeList[Expression](new ClassExpr(resourceType)))
            )),
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
            getStatusCodeMethod.setBody(new BlockStmt(new NodeList(
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "statusCode"))
            )))

            cls
          }

          val responseClasses = responses.value.map { response =>
            val clsName: String = response.statusCodeName.asString
            val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, clsName)
            cls.setExtendedTypes(new NodeList(abstractResponseClassType))

            val (fields, constructor, methods) = response.value.fold({
              val constructor = new ConstructorDeclaration(util.EnumSet.of(PUBLIC), clsName)
              constructor.setBody(new BlockStmt(new NodeList(
                new ExpressionStmt(new MethodCallExpr("super", new IntegerLiteralExpr(response.statusCode)))
              )))
              (List.empty[FieldDeclaration], constructor, List.empty[MethodDeclaration])
            })({ case (valueType, _) =>
              val unboxedValueType: Type = valueType.unbox
              val valueField = new FieldDeclaration(util.EnumSet.of(PRIVATE, FINAL), new VariableDeclarator(unboxedValueType, "value"))

              val constructor = new ConstructorDeclaration(util.EnumSet.of(PUBLIC), clsName)
              constructor.addParameter(new Parameter(util.EnumSet.of(FINAL), unboxedValueType, new SimpleName("value")))
              constructor.setBody(new BlockStmt(new NodeList(
                new ExpressionStmt(new MethodCallExpr("super", new IntegerLiteralExpr(response.statusCode))),
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, "value"), new NameExpr("value"), AssignExpr.Operator.ASSIGN))
              )))

              val getValueMethod = new MethodDeclaration(util.EnumSet.of(PUBLIC), unboxedValueType, "getValue")
              getValueMethod.setBody(new BlockStmt(new NodeList(
                new ReturnStmt(new FieldAccessExpr(new ThisExpr, "value"))
              )))

              (valueField :: Nil, constructor, getValueMethod :: Nil)
            })
            (fields ++ Option(constructor) ++ methods).foreach(cls.addMember)

            cls
          }
          responseClasses.foreach(abstractResponseClass.addMember)

          abstractResponseClass :: Nil
        }

      case RenderClass(className, handlerName, classAnnotations, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        def doRender: Target[List[BodyDeclaration[_]]] = {
          val cls = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, className)
          classAnnotations.foreach(cls.addAnnotation)
          supportDefinitions.foreach(cls.addMember)
          combinedRouteTerms.foreach({
            case bd: BodyDeclaration[_] => cls.addMember(bd)
            case _ =>
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
