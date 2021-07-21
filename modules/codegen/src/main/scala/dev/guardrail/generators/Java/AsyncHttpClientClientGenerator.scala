package dev.guardrail.generators.Java

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.Modifier.Keyword._
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type, UnknownType, VoidType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr.{ MethodCallExpr, NameExpr, _ }
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.{ ImportDeclaration, NodeList }
import dev.guardrail.generators.Java.AsyncHttpClientHelpers._
import dev.guardrail.generators.helpers.DropwizardHelpers
import dev.guardrail.generators.syntax.Java._
import dev.guardrail.generators.{ JavaGenerator, LanguageParameter, LanguageParameters }
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.protocol.terms.client._
import dev.guardrail.protocol.terms._
import dev.guardrail.shims._
import dev.guardrail.terms.collections.CollectionsAbstraction
import dev.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }
import dev.guardrail.{ RenderedClientOperation, StaticDefns, StrictProtocolElems, SupportDefinition, SwaggerUtil, Target }
import java.net.URI
import java.util.concurrent.CompletionStage
import scala.annotation.tailrec

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Null"))
object AsyncHttpClientClientGenerator {
  private val URI_TYPE                       = StaticJavaParser.parseClassOrInterfaceType("URI")
  private val OBJECT_MAPPER_TYPE             = StaticJavaParser.parseClassOrInterfaceType("ObjectMapper")
  private val BUILDER_TYPE                   = StaticJavaParser.parseClassOrInterfaceType("Builder")
  private val MARSHALLING_EXCEPTION_TYPE     = StaticJavaParser.parseClassOrInterfaceType("MarshallingException")
  private val HTTP_ERROR_TYPE                = StaticJavaParser.parseClassOrInterfaceType("HttpError")
  private val EXCEPTION_TYPE                 = StaticJavaParser.parseClassOrInterfaceType("Exception")
  private val JSON_PROCESSING_EXCEPTION_TYPE = StaticJavaParser.parseClassOrInterfaceType("JsonProcessingException")
  private val CLIENT_EXCEPTION_TYPE          = StaticJavaParser.parseClassOrInterfaceType("ClientException")

  private def httpClientFunctionType(implicit Ca: CollectionsAbstraction[JavaLanguage]) = {
    import Ca._
    functionType(REQUEST_TYPE, RESPONSE_TYPE.liftFutureType)
  }

  private def typeReferenceType(typeArg: Type): ClassOrInterfaceType =
    StaticJavaParser.parseClassOrInterfaceType("TypeReference").setTypeArguments(typeArg)

  private def showParam(param: LanguageParameter[JavaLanguage], overrideParamName: Option[String] = None)(
      implicit Ca: CollectionsAbstraction[JavaLanguage]
  ): Expression = {
    val paramName = overrideParamName.getOrElse(param.paramName.asString)
    new MethodCallExpr(
      new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
      "show",
      new NodeList[Expression](new NameExpr(paramName))
    )

    @tailrec
    def doShow(tpe: Type)(implicit Ca: CollectionsAbstraction[JavaLanguage]): Expression = {
      import Ca._
      tpe match {
        case cls: ClassOrInterfaceType if cls.isOptionalType || cls.isListType =>
          doShow(cls.containedType)
        case _ =>
          new MethodCallExpr(
            new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
            "show",
            new NodeList[Expression](new NameExpr(paramName))
          )
      }
    }

    doShow(param.argType)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def generateBuilderMethodCall(param: LanguageParameter[JavaLanguage], builderMethodName: String, needsMultipart: Boolean)(
      implicit Ca: CollectionsAbstraction[JavaLanguage]
  ): Statement = {
    import Ca._

    val finalMethodName = if (needsMultipart) "addBodyPart" else builderMethodName
    val argName         = param.paramName.asString
    val isArray         = param.argType.isListType

    val makeArgList: String => NodeList[Expression] = name =>
      if (param.isFile) {
        new NodeList[Expression](
          new ObjectCreationExpr(
            null,
            INPUT_STREAM_PART_TYPE,
            new NodeList(
              new StringLiteralExpr(param.argName.value),
              new NameExpr(name),
              new StringLiteralExpr(param.argName.value)
            )
          )
        )
      } else if (needsMultipart) {
        new NodeList[Expression](
          new ObjectCreationExpr(
            null,
            STRING_PART_TYPE,
            new NodeList(
              new StringLiteralExpr(param.argName.value),
              showParam(param, Some(name))
            )
          )
        )
      } else {
        new NodeList[Expression](new StringLiteralExpr(param.argName.value), showParam(param, Some(name)))
      }

    if (isArray) {
      new ForEachStmt(
        new VariableDeclarationExpr(param.argType.containedType, "member", finalModifier),
        new NameExpr(argName),
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(new MethodCallExpr(new NameExpr("builder"), finalMethodName, makeArgList("member")))
          )
        )
      )
    } else {
      new ExpressionStmt(new MethodCallExpr(new NameExpr("builder"), finalMethodName, makeArgList(argName)))
    }
  }

  private def generateBodyMethodCall(param: LanguageParameter[JavaLanguage], contentType: Option[ContentType]): Option[Statement] = {
    def wrapSetBody(expr: Expression): MethodCallExpr =
      new MethodCallExpr(new NameExpr("builder"), "setBody", new NodeList[Expression](expr))

    if (param.isFile) {
      Option(new ExpressionStmt(wrapSetBody(new NameExpr(param.paramName.asString))))
    } else {
      contentType match {
        case Some(ApplicationJson) =>
          Option(
            new BlockStmt(
              new NodeList(
                new TryStmt(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(
                        wrapSetBody(
                          new MethodCallExpr(
                            new FieldAccessExpr(new ThisExpr, "objectMapper"),
                            "writeValueAsString",
                            new NodeList[Expression](new NameExpr(param.paramName.asString))
                          )
                        )
                      )
                    )
                  ),
                  new NodeList(
                    new CatchClause(
                      new Parameter(new NodeList(finalModifier), JSON_PROCESSING_EXCEPTION_TYPE, new SimpleName("e")),
                      new BlockStmt(
                        new NodeList(
                          new ThrowStmt(
                            new ObjectCreationExpr(
                              null,
                              MARSHALLING_EXCEPTION_TYPE,
                              new NodeList(new MethodCallExpr(new NameExpr("e"), "getMessage"), new NameExpr("e"))
                            )
                          )
                        )
                      )
                    )
                  ),
                  null
                )
              )
            )
          )

        case Some(TextContent(_)) =>
          Option(
            new ExpressionStmt(
              wrapSetBody(
                new MethodCallExpr(
                  new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
                  "show",
                  new NodeList[Expression](new NameExpr(param.paramName.asString))
                )
              )
            )
          )

        case Some(BinaryContent(_)) | None =>
          // FIXME: we're hoping that the type is something that AHC already supports
          Option(new ExpressionStmt(wrapSetBody(new NameExpr(param.paramName.asString))))

        case Some(UrlencodedFormData) | Some(MultipartFormData) =>
          // We shouldn't be here, since we can't have a body param with these content types
          None
      }
    }
  }

  private def jacksonTypeReferenceFor(tpe: Type): Expression =
    tpe match {
      case cls: ClassOrInterfaceType if cls.getTypeArguments.isPresent =>
        new ObjectCreationExpr(null, typeReferenceType(cls), new NodeList).setAnonymousClassBody(new NodeList)
      case other =>
        new ClassExpr(other)
    }

  private def generateClientExceptionClasses(): Target[List[(List[ImportDeclaration], ClassOrInterfaceDeclaration)]] =
    for {
      httpErrorImports <- List(
        "org.asynchttpclient.Response"
      ).traverse(safeParseRawImport)
    } yield {
      def addStdConstructors(cls: ClassOrInterfaceDeclaration): Unit = {
        val _1 = cls
          .addConstructor(PUBLIC)
          .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("message")))
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new MethodCallExpr("super", new NameExpr("message")))
              )
            )
          )

        val _2 = cls
          .addConstructor(PUBLIC)
          .setParameters(
            new NodeList(
              new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("message")),
              new Parameter(new NodeList(finalModifier), THROWABLE_TYPE, new SimpleName("cause"))
            )
          )
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new MethodCallExpr("super", new NameExpr("message"), new NameExpr("cause")))
              )
            )
          )
      }

      def addNoSerialVersionUuid(cls: ClassOrInterfaceDeclaration): Unit = {
        val _ = cls.addSingleMemberAnnotation("SuppressWarnings", new StringLiteralExpr("serial"))
      }

      val clientExceptionClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, "ClientException")
        .addExtendedType("RuntimeException")
      addStdConstructors(clientExceptionClass)
      addNoSerialVersionUuid(clientExceptionClass)

      val marshallingExceptionClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, "MarshallingException")
        .addExtendedType("ClientException")
      addStdConstructors(marshallingExceptionClass)
      addNoSerialVersionUuid(marshallingExceptionClass)

      val httpErrorClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, "HttpError")
        .addExtendedType("ClientException")
      addNoSerialVersionUuid(httpErrorClass)
      val _1 = httpErrorClass.addField(RESPONSE_TYPE, "response", PRIVATE, FINAL)

      val _2 = httpErrorClass
        .addConstructor(PUBLIC)
        .addParameter(new Parameter(new NodeList(finalModifier), RESPONSE_TYPE, new SimpleName("response")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new MethodCallExpr(
                  "super",
                  new BinaryExpr(
                    new StringLiteralExpr("HTTP server responded with status "),
                    new MethodCallExpr(new NameExpr("response"), "getStatusCode"),
                    BinaryExpr.Operator.PLUS
                  )
                )
              ),
              new ExpressionStmt(
                new AssignExpr(
                  new FieldAccessExpr(new ThisExpr, "response"),
                  new NameExpr("response"),
                  AssignExpr.Operator.ASSIGN
                )
              )
            )
          )
        )

      httpErrorClass
        .addMethod("getResponse", PUBLIC)
        .setType(RESPONSE_TYPE)
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "response"))
            )
          )
        )

      List(
        (List.empty, clientExceptionClass),
        (List.empty, marshallingExceptionClass),
        (httpErrorImports, httpErrorClass)
      )
    }

  def generateAsyncHttpClientSupportClass()(implicit Ca: CollectionsAbstraction[JavaLanguage]): Target[(List[ImportDeclaration], ClassOrInterfaceDeclaration)] =
    for {
      imports <- List(
        "java.util.function.Function",
        "org.asynchttpclient.AsyncHttpClient",
        "org.asynchttpclient.AsyncHttpClientConfig",
        "org.asynchttpclient.DefaultAsyncHttpClient",
        "org.asynchttpclient.DefaultAsyncHttpClientConfig",
        "org.asynchttpclient.Request",
        "org.asynchttpclient.Response"
      ).traverse(safeParseRawImport)
    } yield {
      import Ca._

      val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, "AsyncHttpClientSupport")
      cls.addConstructor(PRIVATE)

      val ahcConfigBuilder = new ObjectCreationExpr(null, DEFAULT_ASYNC_HTTP_CLIENT_CONFIG_BUILDER_TYPE, new NodeList())
      val ahcConfig = new MethodCallExpr(
        List(
          ("setMaxRequestRetry", 2),
          ("setConnectTimeout", 3000),
          ("setRequestTimeout", 10000),
          ("setReadTimeout", 3000),
          ("setMaxConnections", 1024),
          ("setMaxConnectionsPerHost", 1024)
        ).foldLeft[Expression](ahcConfigBuilder)({
          case (lastExpr, (name, arg)) =>
            new MethodCallExpr(lastExpr, name, new NodeList[Expression](new IntegerLiteralExpr(arg.toString)))
        }),
        "build",
        new NodeList[Expression]()
      )

      cls
        .addMethod("createDefaultAsyncHttpClient", PUBLIC, STATIC)
        .setType(ASYNC_HTTP_CLIENT_TYPE)
        .setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(new VariableDeclarationExpr(new VariableDeclarator(ASYNC_HTTP_CLIENT_CONFIG_TYPE, "config", ahcConfig), finalModifier)),
              new ReturnStmt(
                new ObjectCreationExpr(null, DEFAULT_ASYNC_HTTP_CLIENT_TYPE, new NodeList(new NameExpr("config")))
              )
            )
          )
        )

      cls
        .addMethod("createHttpClient", PUBLIC, STATIC)
        .setType(httpClientFunctionType)
        .addParameter(new Parameter(new NodeList(finalModifier), ASYNC_HTTP_CLIENT_TYPE, new SimpleName("client")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new LambdaExpr(
                  new NodeList(new Parameter(new NodeList(finalModifier), REQUEST_TYPE, new SimpleName("request"))),
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new MethodCallExpr(
                        new NameExpr("client"),
                        "executeRequest",
                        new NodeList[Expression](new NameExpr("request"))
                      ),
                      "toCompletableFuture"
                    ).lift[CompletionStage[Any]].toFuture.value
                  ),
                  true
                )
              )
            )
          )
        )

      (imports, cls)
    }

  def generateJacksonSupportClass(): Target[(List[ImportDeclaration], ClassOrInterfaceDeclaration)] =
    for {
      imports <- List(
        "com.fasterxml.jackson.databind.ObjectMapper",
        "com.fasterxml.jackson.datatype.jdk8.Jdk8Module",
        "com.fasterxml.jackson.datatype.jsr310.JavaTimeModule"
      ).traverse(safeParseRawImport)
    } yield {
      val cls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, "JacksonSupport")
      cls.addConstructor(PRIVATE)

      cls
        .addMethod("configureObjectMapper", PUBLIC, STATIC)
        .setType(OBJECT_MAPPER_TYPE)
        .addParameter(new Parameter(new NodeList(finalModifier), OBJECT_MAPPER_TYPE, new SimpleName("objectMapper")))
        .setBody(
          new BlockStmt(
            new NodeList(
              List("JavaTimeModule", "Jdk8Module").map(
                name =>
                  new ExpressionStmt(
                    new MethodCallExpr(
                      new NameExpr("objectMapper"),
                      "registerModule",
                      new NodeList[Expression](new ObjectCreationExpr(null, StaticJavaParser.parseClassOrInterfaceType(name), new NodeList()))
                    )
                  )
              ) :+
                  new ReturnStmt(new NameExpr("objectMapper")): _*
            )
          )
        )

      (imports, cls)
    }

  def ClientTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): ClientTerms[JavaLanguage, Target] =
    new ClientTermInterp

  class ClientTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage])
      extends ClientTerms[JavaLanguage, Target] {
    import Ca._

    implicit def MonadF: Monad[Target] = Target.targetInstances

    def generateClientOperation(
        className: List[String],
        responseClsName: String,
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[JavaLanguage]],
        parameters: LanguageParameters[JavaLanguage]
    )(
        route: RouteMeta,
        methodName: String,
        responses: Responses[JavaLanguage]
    ): Target[RenderedClientOperation[JavaLanguage]] = {
      val RouteMeta(pathStr, httpMethod, operation, securityRequirements) = route

      for {
        callBuilderName    <- JavaGenerator.JavaInterp.formatTypeName(methodName, Some("CallBuilder"))
        responseParentType <- safeParseClassOrInterfaceType(responseClsName)
        callBuilderType    <- safeParseClassOrInterfaceType(callBuilderName)

        pathExprNode <- SwaggerUtil.paths.generateUrlPathParams[JavaLanguage](
          pathStr,
          parameters.pathParams,
          new StringLiteralExpr(_),
          name =>
            new MethodCallExpr(
              new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
              "show",
              new NodeList[Expression](new NameExpr(name.asString))
            ),
          new MethodCallExpr(new FieldAccessExpr(new ThisExpr, "baseUrl"), "toString"),
          (a, b) =>
            (a, b) match {
              case (ae: Expression, be: Expression) => new BinaryExpr(ae, be, BinaryExpr.Operator.PLUS)
              case _                                => a
            }
        )

        pathExpr <- pathExprNode match {
          case e: Expression => Target.pure(e)
          case x =>
            Target.raiseUserError[Expression](s"BUG: Returned node from generateUrlPathParams() was a ${x.getClass.getName}, not an Expression as expected")
        }
      } yield {
        val method = new MethodDeclaration(new NodeList(publicModifier), new VoidType, methodName)
          .setType(callBuilderType)

        parameters.parameters.foreach(p => p.param.setType(p.param.getType.unbox))
        (
          parameters.pathParams ++
              parameters.queryStringParams ++
              parameters.formParams ++
              parameters.headerParams ++
              parameters.bodyParams
        ).filter(_.required).map(_.param).foreach(method.addParameter)

        val requestBuilder = new MethodCallExpr(
          new AssignExpr(
            new VariableDeclarationExpr(new VariableDeclarator(REQUEST_BUILDER_TYPE, "builder"), finalModifier),
            new ObjectCreationExpr(
              null,
              REQUEST_BUILDER_TYPE,
              new NodeList[Expression](
                new StringLiteralExpr(httpMethod.toString)
              )
            ),
            AssignExpr.Operator.ASSIGN
          ),
          "setUrl",
          new NodeList[Expression](pathExpr)
        )

        val builderParamsMethodNames = List(
          (parameters.queryStringParams, "addQueryParam", false),
          (parameters.formParams, "addFormParam", parameters.formParams.exists(_.isFile)),
          (parameters.headerParams, "addHeader", false)
        )

        val allConsumes = operation.unwrapTracker.consumes.flatMap(ContentType.unapply).toList
        val consumes    = DropwizardHelpers.getBestConsumes(operation, allConsumes, parameters)
        val allProduces = operation.unwrapTracker.produces.flatMap(ContentType.unapply).toList
        val produces =
          responses.value
            .map(
              resp => (resp.statusCode, DropwizardHelpers.getBestProduces[JavaLanguage](operation.unwrapTracker.getOperationId, allProduces, resp, _.isPlain))
            )
            .toMap

        val builderMethodCalls: List[(LanguageParameter[JavaLanguage], Statement)] = builderParamsMethodNames
            .flatMap({
              case (params, name, needsMultipart) =>
                params.map(param => (param, generateBuilderMethodCall(param, name, needsMultipart)))
            }) ++
              parameters.bodyParams.flatMap(param => generateBodyMethodCall(param, consumes).map(stmt => (param, stmt)))

        val callBuilderCreation = new ObjectCreationExpr(
          null,
          callBuilderType,
          (List[Expression](
            new NameExpr("builder"),
            new FieldAccessExpr(new ThisExpr, "httpClient"),
            new FieldAccessExpr(new ThisExpr, "objectMapper")
          ) ++ (if (tracing) Option(new FieldAccessExpr(new ThisExpr, "clientName")) else None)).toNodeList
        )

        method.setBody(
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(requestBuilder) +:
                  builderMethodCalls.filter(_._1.required).map(_._2) :+
                  new ReturnStmt(callBuilderCreation): _*
            )
          )
        )

        val callBuilderFinalFields = List(
            (REQUEST_BUILDER_TYPE, "builder"),
            (httpClientFunctionType, "httpClient"),
            (OBJECT_MAPPER_TYPE, "objectMapper")
          ) ++ (if (tracing) Option((STRING_TYPE, "clientName")) else None)

        val callBuilderCls = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, callBuilderName)
        callBuilderFinalFields.foreach({ case (tpe, name) => callBuilderCls.addField(tpe, name, PRIVATE, FINAL) })
        val callBuilderInitContentType = consumes.map({ ct =>
          val ctStr = ct match {
            case TextContent(_) => s"${ct.value}; charset=utf-8"
            case _              => ct.value
          }
          new ExpressionStmt(
            new MethodCallExpr(
              "withHeader",
              new StringLiteralExpr("Content-Type"),
              new StringLiteralExpr(ctStr)
            )
          )
        })
        val callBuilderInitAccept = {
          val acceptHeaderString = produces
            .flatMap({ case (_, ct) => ct.map(ct => s"${ct.value}; q=1.0") })
            .mkString(", ")
          if (acceptHeaderString.nonEmpty) {
            Option(
              new ExpressionStmt(
                new MethodCallExpr(
                  "withHeader",
                  new StringLiteralExpr("Accept"),
                  new StringLiteralExpr(s"$acceptHeaderString, */*; q=0.1")
                )
              )
            )
          } else {
            Option.empty
          }
        }

        callBuilderCls
          .addConstructor(PRIVATE)
          .setParameters(
            callBuilderFinalFields.map({ case (tpe, name) => new Parameter(new NodeList(finalModifier), tpe, new SimpleName(name)) }).toNodeList
          )
          .setBody(
            new BlockStmt(
              (
                callBuilderFinalFields
                  .map({
                    case (_, name) =>
                      new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, name), new NameExpr(name), AssignExpr.Operator.ASSIGN)): Statement
                  }) ++ callBuilderInitContentType ++ callBuilderInitAccept
              ).toNodeList
            )
          )

        val optionalParamMethods = builderMethodCalls
          .filterNot(_._1.required)
          .flatMap({
            case (LanguageParameter(_, param, _, _, argType), methodCall) =>
              val containedType = argType.containedType.unbox

              val optionalOverrideMethod = if (argType.isOptionalType && !containedType.isOptionalType) {
                val methodParamName = s"optional${param.getNameAsString.capitalize}"

                Some(
                  new MethodDeclaration(
                    new NodeList(publicModifier),
                    s"with${param.getNameAsString.unescapeIdentifier.capitalize}",
                    callBuilderType,
                    List(
                      new Parameter(new NodeList(finalModifier), argType, new SimpleName(methodParamName))
                    ).toNodeList
                  ).setBody(
                    new BlockStmt(
                      List(
                        new ExpressionStmt(
                          new NameExpr(methodParamName)
                            .lift[Option[Any]]
                            .foreach(
                              new LambdaExpr(new NodeList(new Parameter(new UnknownType, param.getNameAsString)), methodCall.clone(), false).lift[Any => Unit]
                            )
                            .value
                        ),
                        new ReturnStmt(new ThisExpr)
                      ).toNodeList
                    )
                  )
                )
              } else {
                Option.empty[MethodDeclaration]
              }

              val mainMethod = new MethodDeclaration(
                new NodeList(publicModifier),
                s"with${param.getNameAsString.unescapeIdentifier.capitalize}",
                callBuilderType,
                List(
                  new Parameter(new NodeList(finalModifier), containedType, new SimpleName(param.getNameAsString))
                ).toNodeList
              ).setBody(
                new BlockStmt(
                  List(
                    methodCall,
                    new ReturnStmt(new ThisExpr)
                  ).toNodeList
                )
              )

              mainMethod +: optionalOverrideMethod.toList
          })
        optionalParamMethods.foreach(callBuilderCls.addMember)

        callBuilderCls
          .addMethod("withHeader", PUBLIC)
          .setParameters(
            List(
              new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")),
              new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("value"))
            ).toNodeList
          )
          .setType(callBuilderType)
          .setBody(
            new BlockStmt(
              List(
                new ExpressionStmt(
                  new MethodCallExpr(
                    new FieldAccessExpr(new ThisExpr, "builder"),
                    "addHeader",
                    List[Expression](new NameExpr("name"), new NameExpr("value")).toNodeList
                  )
                ),
                new ReturnStmt(new ThisExpr)
              ).toNodeList
            )
          )

        val httpMethodCallExpr = new MethodCallExpr(
          new FieldAccessExpr(new ThisExpr, "httpClient"),
          "apply",
          new NodeList[Expression](new MethodCallExpr(new NameExpr("builder"), "build"))
        )
        val requestCall =
          httpMethodCallExpr
            .lift[CompletionStage[Any]]
            .toFuture
            .map(
              new LambdaExpr(
                new Parameter(new UnknownType, "response"),
                new BlockStmt(
                  new NodeList(
                    new SwitchStmt(
                      new MethodCallExpr(new NameExpr("response"), "getStatusCode"),
                      new NodeList(
                        responses.value.map(
                          response =>
                            new SwitchEntry(
                              new NodeList(new IntegerLiteralExpr(response.statusCode.toString)),
                              SwitchEntry.Type.BLOCK,
                              new NodeList(response.value match {
                                case None =>
                                  new ReturnStmt(
                                    new ObjectCreationExpr(
                                      null,
                                      StaticJavaParser.parseClassOrInterfaceType(s"$responseClsName.${response.statusCodeName.asString}"),
                                      new NodeList()
                                    )
                                  )
                                case Some((contentType, valueType, _)) =>
                                  new TryStmt(
                                    new BlockStmt(
                                      new NodeList(
                                        new ExpressionStmt(
                                          produces
                                            .get(response.statusCode)
                                            .flatten
                                            .getOrElse({
                                              println(
                                                s"WARNING: no supported content type specified at ${operation.showHistory}'s ${response.statusCode} response; falling back to application/json"
                                              )
                                              ApplicationJson
                                            }) match {
                                            case ApplicationJson =>
                                              new AssignExpr(
                                                new VariableDeclarationExpr(new VariableDeclarator(valueType, "result"), finalModifier),
                                                new MethodCallExpr(
                                                  new FieldAccessExpr(new ThisExpr, "objectMapper"),
                                                  "readValue",
                                                  new NodeList[Expression](
                                                    new MethodCallExpr(new NameExpr("response"), "getResponseBodyAsStream"),
                                                    jacksonTypeReferenceFor(valueType)
                                                  )
                                                ),
                                                AssignExpr.Operator.ASSIGN
                                              )

                                            case contentType =>
                                              val bodyGetter = valueType match {
                                                case _ if valueType.isNamed("InputStream") => "getResponseBodyAsStream"
                                                case _ if valueType.isNamed("byte[]")      => "getResponseBodyAsBytes"
                                                case _ if valueType.isNamed("ByteBuffer")  => "getResponseBodyAsByteBuffer"
                                                case _ if valueType.isNamed("String")      => "getResponseBody"
                                                case _ =>
                                                  println(
                                                    s"WARNING: Don't know how to handle response of type ${valueType.asString} for content type $contentType at ${operation.showHistory}; falling back to String"
                                                  )
                                                  "getResponseBody"
                                              }
                                              val bodyGetterExpr = new MethodCallExpr(
                                                new NameExpr("response"),
                                                bodyGetter,
                                                bodyGetter match {
                                                  case "getResponseBody" =>
                                                    new NodeList[Expression](
                                                      new MethodCallExpr(
                                                        new MethodCallExpr(
                                                          new NameExpr("AsyncHttpClientUtils"),
                                                          "getResponseCharset",
                                                          new NodeList[Expression](new NameExpr("response"))
                                                        ),
                                                        "orElse",
                                                        new NodeList[Expression](new FieldAccessExpr(new NameExpr("StandardCharsets"), "UTF_8"))
                                                      )
                                                    )
                                                  case _ => new NodeList[Expression]
                                                }
                                              )
                                              new AssignExpr(
                                                new VariableDeclarationExpr(new VariableDeclarator(valueType, "result"), finalModifier),
                                                bodyGetterExpr,
                                                AssignExpr.Operator.ASSIGN
                                              )
                                          }
                                        ),
                                        new IfStmt(
                                          new BinaryExpr(new NameExpr("result"), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                                          new BlockStmt(
                                            new NodeList(
                                              new ThrowStmt(
                                                new ObjectCreationExpr(
                                                  null,
                                                  MARSHALLING_EXCEPTION_TYPE,
                                                  new NodeList(new StringLiteralExpr("Failed to unmarshal response"))
                                                )
                                              )
                                            )
                                          ),
                                          new BlockStmt(
                                            new NodeList(
                                              new ReturnStmt(
                                                new ObjectCreationExpr(
                                                  null,
                                                  StaticJavaParser.parseClassOrInterfaceType(s"$responseClsName.${response.statusCodeName.asString}"),
                                                  new NodeList[Expression](new NameExpr("result"))
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    new NodeList(
                                      new CatchClause(
                                        new Parameter(new NodeList(finalModifier), MARSHALLING_EXCEPTION_TYPE, new SimpleName("e")),
                                        new BlockStmt(
                                          new NodeList(
                                            new ThrowStmt(new NameExpr("e"))
                                          )
                                        )
                                      ),
                                      new CatchClause(
                                        new Parameter(new NodeList(finalModifier), EXCEPTION_TYPE, new SimpleName("e")),
                                        new BlockStmt(
                                          new NodeList(
                                            new ThrowStmt(
                                              new ObjectCreationExpr(
                                                null,
                                                MARSHALLING_EXCEPTION_TYPE,
                                                new NodeList(new MethodCallExpr(new NameExpr("e"), "getMessage"), new NameExpr("e"))
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    null
                                  )
                              })
                            )
                        ) :+ new SwitchEntry(
                              new NodeList(),
                              SwitchEntry.Type.BLOCK,
                              new NodeList(new ThrowStmt(new ObjectCreationExpr(null, HTTP_ERROR_TYPE, new NodeList(new NameExpr("response")))))
                            ): _*
                      )
                    )
                  )
                )
              ).lift[Any => Any]
            )
            .value

        callBuilderCls
          .addMethod("call", PUBLIC)
          .setType(responseParentType.liftFutureType)
          .addThrownException(CLIENT_EXCEPTION_TYPE)
          .setBody(new BlockStmt(List[Statement](new ReturnStmt(requestCall)).toNodeList))

        RenderedClientOperation[JavaLanguage](method, callBuilderCls :: Nil)
      }
    }
    def getImports(tracing: Boolean): Target[List[com.github.javaparser.ast.ImportDeclaration]] =
      if (tracing) {
        Target.raiseUserError("Tracing is not yet supported by this framework")
      } else {
        (List(
          "java.net.URI",
          "java.nio.charset.StandardCharsets",
          "java.util.function.Function",
          "java.util.function.Supplier",
          "com.fasterxml.jackson.core.JsonProcessingException",
          "com.fasterxml.jackson.core.type.TypeReference",
          "com.fasterxml.jackson.databind.ObjectMapper",
          "org.asynchttpclient.Request",
          "org.asynchttpclient.RequestBuilder",
          "org.asynchttpclient.Response",
          "org.asynchttpclient.request.body.multipart.InputStreamPart",
          "org.asynchttpclient.request.body.multipart.StringPart"
        ).map(safeParseRawImport) ++ List(
              "java.util.Objects.requireNonNull"
            ).map(safeParseRawStaticImport)).sequence
      }
    def getExtraImports(tracing: Boolean): Target[List[com.github.javaparser.ast.ImportDeclaration]] = Target.pure(List.empty)
    def clientClsArgs(
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        tracing: Boolean
    ): Target[List[List[com.github.javaparser.ast.body.Parameter]]] =
      Target.pure(List.empty)

    def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[JavaLanguage],
        protocolElems: List[StrictProtocolElems[JavaLanguage]]
    ): Target[List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]]] = {
      val genericTypeParam = StaticJavaParser.parseClassOrInterfaceType("T")

      val responseData = responses.value.map({
        case Response(statusCodeName, valueType, _) =>
          val responseName: String = statusCodeName.asString
          val responseType         = StaticJavaParser.parseClassOrInterfaceType(responseName)
          val responseLambdaName   = s"handle${responseName}"

          val responseInnerClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, responseName);
          responseInnerClass.addExtendedType(responseClsName)
          val (foldMethodParamType, foldMethodApplier, foldMethodArgs) = valueType.fold(
            (supplierType(genericTypeParam), "get", new NodeList[Expression]())
          )({
            vt =>
              val finalValueType: Type = vt.unbox

              responseInnerClass.addField(finalValueType, "value", PRIVATE, FINAL)

              val constructor = responseInnerClass.addConstructor(PUBLIC)
              constructor.addParameter(new Parameter(new NodeList(finalModifier), finalValueType, new SimpleName("value")))
              constructor.setBody(
                new BlockStmt(
                  new NodeList(
                    new ExpressionStmt(
                      new AssignExpr(
                        new FieldAccessExpr(new ThisExpr, "value"),
                        new NameExpr("value"),
                        AssignExpr.Operator.ASSIGN
                      )
                    )
                  )
                )
              )

              val getValueMethod = responseInnerClass.addMethod("getValue", PUBLIC)
              getValueMethod.setType(finalValueType)
              getValueMethod.setBody(
                new BlockStmt(
                  new NodeList(
                    new ReturnStmt(new FieldAccessExpr(new ThisExpr, "value"))
                  )
                )
              )

              (
                functionType(vt, genericTypeParam),
                "apply",
                new NodeList[Expression](
                  new MethodCallExpr(new EnclosedExpr(new CastExpr(responseType, new ThisExpr)), "getValue")
                )
              )
          })

          val foldMethodParameter = new Parameter(new NodeList(finalModifier), foldMethodParamType, new SimpleName(responseLambdaName))

          val foldMethodBranch = new IfStmt(
            new InstanceOfExpr(new ThisExpr, responseType),
            new BlockStmt(
              new NodeList(
                new ReturnStmt(
                  new MethodCallExpr(
                    new NameExpr(responseLambdaName),
                    foldMethodApplier,
                    foldMethodArgs
                  )
                )
              )
            ),
            null
          )

          (responseInnerClass, foldMethodParameter, foldMethodBranch)
      })

      val abstractResponseClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, responseClsName)

      val (innerClasses, foldMethodParameters, foldMethodIfBranches) = responseData.unzip3

      innerClasses.foreach(abstractResponseClass.addMember)

      val foldMethod = abstractResponseClass
        .addMethod("fold", PUBLIC)
        .addTypeParameter("T")
        .setType("T")
      foldMethodParameters.foreach(foldMethod.addParameter)

      NonEmptyList
        .fromList(foldMethodIfBranches)
        .foreach({ nel =>
          nel.reduceLeft({ (prev, next) =>
            prev.setElseStmt(next)
            next
          })

          nel.last.setElseStmt(
            new BlockStmt(
              new NodeList(
                new ThrowStmt(new ObjectCreationExpr(null, ASSERTION_ERROR_TYPE, new NodeList(new StringLiteralExpr("This is a bug in guardrail!"))))
              )
            )
          )

          foldMethod.setBody(new BlockStmt(new NodeList(nel.head)))
        })

      Target.pure(List(abstractResponseClass))
    }
    def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[JavaLanguage]]
    ): Target[List[SupportDefinition[JavaLanguage]]] =
      for {
        exceptionClasses <- generateClientExceptionClasses()
        ahcSupport       <- generateAsyncHttpClientSupportClass()
        (ahcSupportImports, ahcSupportClass) = ahcSupport
        jacksonSupport <- generateJacksonSupportClass()
        (jacksonSupportImports, jacksonSupportClass) = jacksonSupport
        asyncHttpclientUtils <- asyncHttpClientUtilsSupportDef
      } yield {
        exceptionClasses.map({
          case (imports, cls) =>
            SupportDefinition[JavaLanguage](new Name(cls.getNameAsString), imports, List(cls))
        }) ++ List(
          SupportDefinition[JavaLanguage](new Name(ahcSupportClass.getNameAsString), ahcSupportImports, List(ahcSupportClass)),
          SupportDefinition[JavaLanguage](new Name(jacksonSupportClass.getNameAsString), jacksonSupportImports, List(jacksonSupportClass)),
          asyncHttpclientUtils
        )
      }
    def buildStaticDefns(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        ctorArgs: List[List[com.github.javaparser.ast.body.Parameter]],
        tracing: Boolean
    ): Target[StaticDefns[JavaLanguage]] =
      Target.pure(
        StaticDefns[JavaLanguage](
          className = clientName,
          extraImports = List.empty,
          definitions = List.empty
        )
      )

    def buildClient(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        basePath: Option[String],
        ctorArgs: List[List[com.github.javaparser.ast.body.Parameter]],
        clientCalls: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
        supportDefinitions: List[com.github.javaparser.ast.body.BodyDeclaration[_ <: com.github.javaparser.ast.body.BodyDeclaration[_]]],
        tracing: Boolean
    ): Target[NonEmptyList[Either[
      com.github.javaparser.ast.body.ClassOrInterfaceDeclaration,
      com.github.javaparser.ast.body.TypeDeclaration[_ <: com.github.javaparser.ast.body.TypeDeclaration[_]]
    ]]] = {
      def createSetter(tpe: Type, name: String, initializer: String => Target[Expression]): Target[MethodDeclaration] =
        for {
          initializerExpr <- initializer(name)
        } yield new MethodDeclaration(new NodeList(publicModifier), BUILDER_TYPE, s"with${name.capitalize}")
          .addParameter(new Parameter(new NodeList(finalModifier), tpe, new SimpleName(name)))
          .setBody(
            new BlockStmt(
              new NodeList(
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, name), initializerExpr, AssignExpr.Operator.ASSIGN)),
                new ReturnStmt(new ThisExpr)
              )
            )
          )
      val nonNullInitializer: String => Target[Expression] = name => Target.pure(requireNonNullExpr(name))
      def optionalInitializer(valueArg: String => Expression): String => Target[Expression] =
        name => Cl.liftOptionalTerm(valueArg(name)).flatMap(_.toExpression)

      val serverUrl = serverUrls.map(_.head).map(uri => new URI(uri.toString + basePath.getOrElse("")))

      for {
        optionalHttpClientFunctionType <- Cl.liftOptionalType(httpClientFunctionType)
        optionalObjectMapperType       <- Cl.liftOptionalType(OBJECT_MAPPER_TYPE)
        emptyOptionalTerm              <- Cl.emptyOptionalTerm().flatMap(_.toExpression)

        builderBaseUrlSetter    <- serverUrl.traverse(_ => createSetter(URI_TYPE, "baseUrl", nonNullInitializer))
        builderClientNameSetter <- tracingName.filter(_ => tracing).traverse(_ => createSetter(STRING_TYPE, "clientName", nonNullInitializer))
        builderHttpClientSetter <- createSetter(httpClientFunctionType, "httpClient", optionalInitializer(new NameExpr(_)))
        builderObjectMapperSetter <- createSetter(
          OBJECT_MAPPER_TYPE,
          "objectMapper",
          optionalInitializer(
            name => new MethodCallExpr(new NameExpr("JacksonSupport"), "configureObjectMapper", new NodeList[Expression](new NameExpr(name)))
          )
        )
      } yield {
        val clientType = StaticJavaParser.parseClassOrInterfaceType(clientName)

        val (baseUrlField, defaultBaseUrlField) = {
          val (modifiers, declarator) = serverUrl.fold(
            (new NodeList(privateModifier, finalModifier), new VariableDeclarator(URI_TYPE, "baseUrl"))
          )(
            _ => (new NodeList(privateModifier), new VariableDeclarator(URI_TYPE, "baseUrl", new NameExpr("DEFAULT_BASE_URL")))
          )
          (
            new FieldDeclaration(modifiers, declarator),
            serverUrl.map({ serverUrl =>
              new FieldDeclaration(
                new NodeList(privateModifier, staticModifier, finalModifier),
                new VariableDeclarator(
                  URI_TYPE,
                  "DEFAULT_BASE_URL",
                  new MethodCallExpr(
                    new NameExpr("URI"),
                    "create",
                    new NodeList[Expression](new StringLiteralExpr(serverUrl.toString))
                  )
                )
              )
            })
          )
        }

        val tracingFields = if (tracing) {
          val (modifiers, declarator) = tracingName.fold(
            (new NodeList(privateModifier, finalModifier), new VariableDeclarator(STRING_TYPE, "clientName"))
          )(
            _ => (new NodeList(privateModifier), new VariableDeclarator(STRING_TYPE, "clientName", new NameExpr("DEFAULT_CLIENT_NAME")))
          )
          val clientNameField = new FieldDeclaration(modifiers, declarator)
          val defaultClientNameField = tracingName.map({ tracingName =>
            new FieldDeclaration(
              new NodeList(privateModifier, staticModifier, finalModifier),
              new VariableDeclarator(
                STRING_TYPE,
                "DEFAULT_CLIENT_NAME",
                new StringLiteralExpr(tracingName)
              )
            )
          })

          defaultClientNameField.toList :+ clientNameField
        } else {
          List.empty
        }

        val httpClientField = new FieldDeclaration(
          new NodeList(privateModifier),
          new VariableDeclarator(
            optionalHttpClientFunctionType,
            "httpClient",
            emptyOptionalTerm
          )
        )
        val objectMapperField = new FieldDeclaration(
          new NodeList(privateModifier),
          new VariableDeclarator(
            optionalObjectMapperType,
            "objectMapper",
            emptyOptionalTerm
          )
        )

        val builderConstructor = new ConstructorDeclaration(new NodeList(publicModifier), "Builder")
        def createConstructorParameter(tpe: Type, name: String): Parameter =
          new Parameter(new NodeList(finalModifier), tpe, new SimpleName(name))
        def createBuilderConstructorAssignment(name: String): Statement =
          new ExpressionStmt(
            new AssignExpr(new FieldAccessExpr(new ThisExpr, name), requireNonNullExpr(name), AssignExpr.Operator.ASSIGN)
          )
        (serverUrl, tracingName) match {
          case (None, None) if tracing =>
            builderConstructor.setParameters(
              new NodeList(
                createConstructorParameter(URI_TYPE, "baseUrl"),
                createConstructorParameter(STRING_TYPE, "clientName")
              )
            )
            builderConstructor.setBody(
              new BlockStmt(
                new NodeList(
                  createBuilderConstructorAssignment("baseUrl"),
                  createBuilderConstructorAssignment("clientName")
                )
              )
            )

          case (Some(_), None) if tracing =>
            builderConstructor.setParameters(
              new NodeList(
                createConstructorParameter(STRING_TYPE, "clientName")
              )
            )
            builderConstructor.setBody(
              new BlockStmt(
                new NodeList(
                  createBuilderConstructorAssignment("clientName")
                )
              )
            )

          case (None, _) =>
            builderConstructor.setParameters(
              new NodeList(
                createConstructorParameter(URI_TYPE, "baseUrl")
              )
            )
            builderConstructor.setBody(
              new BlockStmt(
                new NodeList(
                  createBuilderConstructorAssignment("baseUrl")
                )
              )
            )

          case (Some(_), Some(_)) => // no params

          case (Some(_), _) if !tracing => // no params
        }

        val builderSetters = List(
          builderBaseUrlSetter,
          builderClientNameSetter,
          Some(builderHttpClientSetter),
          Some(builderObjectMapperSetter)
        ).flatten

        val buildMethod = new MethodDeclaration(new NodeList(publicModifier), clientType, "build")
          .setBody(
            new BlockStmt(
              new NodeList(
                new ReturnStmt(new ObjectCreationExpr(null, clientType, new NodeList(new ThisExpr)))
              )
            )
          )

        def createInternalGetter(tpe: Type, name: String, getterCall: Expression): MethodDeclaration =
          new MethodDeclaration(new NodeList(privateModifier), tpe, s"get${name.capitalize}")
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ReturnStmt(
                    new FieldAccessExpr(new ThisExpr, name)
                      .lift[Option[Any]]
                      .getOrElse(new LambdaExpr(new NodeList[Parameter], getterCall).lift[() => Any])
                      .value
                  )
                )
              )
            )
        val internalGetters = List(
          createInternalGetter(
            httpClientFunctionType,
            "httpClient",
            new MethodCallExpr(
              new NameExpr("AsyncHttpClientSupport"),
              "createHttpClient",
              new NodeList[Expression](
                new MethodCallExpr(new NameExpr("AsyncHttpClientSupport"), "createDefaultAsyncHttpClient")
              )
            )
          ),
          createInternalGetter(
            OBJECT_MAPPER_TYPE,
            "objectMapper",
            new MethodCallExpr(
              new NameExpr("JacksonSupport"),
              "configureObjectMapper",
              new NodeList[Expression](new ObjectCreationExpr(null, OBJECT_MAPPER_TYPE, new NodeList()))
            )
          )
        )

        val builderClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, "Builder")
        sortDefinitions(
          tracingFields ++
              builderSetters ++
              internalGetters ++
              defaultBaseUrlField ++
              List(
                baseUrlField,
                httpClientField,
                objectMapperField,
                builderConstructor,
                buildMethod
              )
        ).foreach(builderClass.addMember)

        val clientFields = List(
          Some((URI_TYPE, "baseUrl")),
          if (tracing) Some((STRING_TYPE, "clientName")) else None,
          Some((httpClientFunctionType, "httpClient")),
          Some((OBJECT_MAPPER_TYPE, "objectMapper"))
        ).flatten.map({
          case (tpe, name) =>
            new FieldDeclaration(new NodeList(privateModifier, finalModifier), new VariableDeclarator(tpe, name))
        })

        val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clientName)
        constructor.addParameter(new Parameter(new NodeList(finalModifier), BUILDER_TYPE, new SimpleName("builder")))
        def newFieldAccessExpr(scope: Expression, name: String): Expression = new FieldAccessExpr(scope, name)
        def newMethodCallExpr(scope: Expression, name: String): Expression  = new MethodCallExpr(scope, s"get${name.capitalize}")
        constructor.setBody(
          new BlockStmt(
            new NodeList(
              List[Option[(String, (Expression, String) => Expression)]](
                Some(("baseUrl", newFieldAccessExpr)),
                if (tracing) Some(("clientName", newFieldAccessExpr)) else None,
                Some(("httpClient", newMethodCallExpr)),
                Some(("objectMapper", newMethodCallExpr))
              ).flatten.map({
                case (name, value) =>
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, name),
                      value(new NameExpr("builder"), name),
                      AssignExpr.Operator.ASSIGN
                    )
                  )
              }): _*
            )
          )
        )

        val clientClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, clientName)
        sortDefinitions(
          List(
            builderClass,
            constructor
          ) ++
              clientCalls ++
              clientFields ++
              supportDefinitions
        ).foreach(clientClass.addMember)

        NonEmptyList(Right(clientClass), Nil)
      }
    }
  }
}
