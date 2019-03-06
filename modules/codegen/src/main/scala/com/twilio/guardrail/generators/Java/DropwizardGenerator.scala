package com.twilio.guardrail.generators.Java

import cats.instances.list._
import cats.syntax.traverse._
import cats.~>
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Modifier.{ABSTRACT, FINAL, PRIVATE, PUBLIC}
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, Parameter}
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.{BlockStmt, ExpressionStmt, ReturnStmt}
import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms.framework._
import java.util

object DropwizardGenerator {
  private val RESPONSE_TYPE = JavaParser.parseClassOrInterfaceType("org.asynchttpclient.Response")

  object FrameworkInterp extends (FrameworkTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: FrameworkTerm[JavaLanguage, T]): Target[T] = term match {
      case FileType(format) => safeParseType(format.getOrElse("java.io.File"))
      case ObjectType(format) => safeParseType("com.fasterxml.jackson.databind.JsonNode")

      case GetFrameworkImports(tracing) =>
        Target.pure(List.empty)

      case GetFrameworkImplicits() =>
        Target.pure(None)

      case GetFrameworkDefinitions() =>
        def addStdConstructors(cls: ClassOrInterfaceDeclaration): Unit = {
          val msgConstructor = cls.addConstructor(PUBLIC)
          msgConstructor.addParameter(new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("message")))
          msgConstructor.setBody(new BlockStmt(new NodeList(
            new ExpressionStmt(new MethodCallExpr("super", new NameExpr("message")))
          )))

          val msgCauseConstructor = cls.addConstructor(PUBLIC)
          msgCauseConstructor.setParameters(new NodeList(
            new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("message")),
            new Parameter(util.EnumSet.of(FINAL), THROWABLE_TYPE, new SimpleName("cause"))
          ))
          msgCauseConstructor.setBody(new BlockStmt(new NodeList(
            new ExpressionStmt(new MethodCallExpr("super", new NameExpr("message"), new NameExpr("cause")))
          )))
        }

        val clientExceptionClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, ABSTRACT), false, "ClientException")
        clientExceptionClass.addExtendedType("RuntimeException")
        addStdConstructors(clientExceptionClass)

        val marshallingExceptionClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, "MarshallingException")
        marshallingExceptionClass.addExtendedType("ClientException")
        addStdConstructors(marshallingExceptionClass)

        val httpErrorClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, "HttpError")
        httpErrorClass.addExtendedType("ClientException")
        httpErrorClass.addField(RESPONSE_TYPE, "response", PRIVATE, FINAL)

        val responseConstructor = httpErrorClass.addConstructor(PUBLIC)
        responseConstructor.addParameter(new Parameter(util.EnumSet.of(FINAL), RESPONSE_TYPE, new SimpleName("response")))
        responseConstructor.setBody(new BlockStmt(new NodeList(
          new ExpressionStmt(new MethodCallExpr(
            "super",
            new BinaryExpr(
              new StringLiteralExpr("HTTP server responded with status "),
              new MethodCallExpr(new NameExpr("response"), "getStatusCode"),
              BinaryExpr.Operator.PLUS
            )
          )),
          new ExpressionStmt(new AssignExpr(
            new FieldAccessExpr(new ThisExpr, "response"),
            new NameExpr("response"),
            AssignExpr.Operator.ASSIGN
          ))
        )))

        val getResponseMethod = httpErrorClass.addMethod("getResponse", PUBLIC)
        getResponseMethod.setType(RESPONSE_TYPE)
        getResponseMethod.setBody(new BlockStmt(new NodeList(
          new ReturnStmt(new FieldAccessExpr(new ThisExpr, "response"))
        )))

        val showerClass = SHOWER_CLASS_DEF

        Target.pure(List(
          (new Name("ClientException"), clientExceptionClass),
          (new Name("MarshallingException"), marshallingExceptionClass),
          (new Name("HttpError"), httpErrorClass),
          (new Name(showerClass.getNameAsString), showerClass)
        ))

      case LookupStatusCode(key) =>
        def parseStatusCode(code: Int, termName: String): Target[(Int, Name)] =
          safeParseName(termName).map(name => (code, name))

        key match {
          case "100" => parseStatusCode(100, "Continue")
          case "101" => parseStatusCode(101, "SwitchingProtocols")
          case "102" => parseStatusCode(102, "Processing")

          case "200" => parseStatusCode(200, "Ok")
          case "201" => parseStatusCode(201, "Created")
          case "202" => parseStatusCode(202, "Accepted")
          case "203" => parseStatusCode(203, "NonAuthoritativeInformation")
          case "204" => parseStatusCode(204, "NoContent")
          case "205" => parseStatusCode(205, "ResetContent")
          case "206" => parseStatusCode(206, "PartialContent")
          case "207" => parseStatusCode(207, "MultiStatus")
          case "208" => parseStatusCode(208, "AlreadyReported")
          case "226" => parseStatusCode(226, "IMUsed")

          case "300" => parseStatusCode(300, "MultipleChoices")
          case "301" => parseStatusCode(301, "MovedPermanently")
          case "302" => parseStatusCode(302, "Found")
          case "303" => parseStatusCode(303, "SeeOther")
          case "304" => parseStatusCode(304, "NotModified")
          case "305" => parseStatusCode(305, "UseProxy")
          case "307" => parseStatusCode(307, "TemporaryRedirect")
          case "308" => parseStatusCode(308, "PermanentRedirect")

          case "400" => parseStatusCode(400, "BadRequest")
          case "401" => parseStatusCode(401, "Unauthorized")
          case "402" => parseStatusCode(402, "PaymentRequired")
          case "403" => parseStatusCode(403, "Forbidden")
          case "404" => parseStatusCode(404, "NotFound")
          case "405" => parseStatusCode(405, "MethodNotAllowed")
          case "406" => parseStatusCode(406, "NotAcceptable")
          case "407" => parseStatusCode(407, "ProxyAuthenticationRequired")
          case "408" => parseStatusCode(408, "RequestTimeout")
          case "409" => parseStatusCode(409, "Conflict")
          case "410" => parseStatusCode(410, "Gone")
          case "411" => parseStatusCode(411, "LengthRequired")
          case "412" => parseStatusCode(412, "PreconditionFailed")
          case "413" => parseStatusCode(413, "RequestEntityTooLarge")
          case "414" => parseStatusCode(414, "RequestUriTooLong")
          case "415" => parseStatusCode(415, "UnsupportedMediaType")
          case "416" => parseStatusCode(416, "RequestedRangeNotSatisfiable")
          case "417" => parseStatusCode(417, "ExpectationFailed")
          case "418" => parseStatusCode(418, "ImATeapot")
          case "420" => parseStatusCode(420, "EnhanceYourCalm")
          case "422" => parseStatusCode(422, "UnprocessableEntity")
          case "423" => parseStatusCode(423, "Locked")
          case "424" => parseStatusCode(424, "FailedDependency")
          case "425" => parseStatusCode(425, "UnorderedCollection")
          case "426" => parseStatusCode(426, "UpgradeRequired")
          case "428" => parseStatusCode(428, "PreconditionRequired")
          case "429" => parseStatusCode(429, "TooManyRequests")
          case "431" => parseStatusCode(431, "RequestHeaderFieldsTooLarge")
          case "449" => parseStatusCode(449, "RetryWith")
          case "450" => parseStatusCode(450, "BlockedByParentalControls")
          case "451" => parseStatusCode(451, "UnavailableForLegalReasons")

          case "500" => parseStatusCode(500, "InternalServerError")
          case "501" => parseStatusCode(501, "NotImplemented")
          case "502" => parseStatusCode(502, "BadGateway")
          case "503" => parseStatusCode(503, "ServiceUnavailable")
          case "504" => parseStatusCode(504, "GatewayTimeout")
          case "505" => parseStatusCode(505, "HTTPVersionNotSupported")
          case "506" => parseStatusCode(506, "VariantAlsoNegotiates")
          case "507" => parseStatusCode(507, "InsufficientStorage")
          case "508" => parseStatusCode(508, "LoopDetected")
          case "509" => parseStatusCode(509, "BandwidthLimitExceeded")
          case "510" => parseStatusCode(510, "NotExtended")
          case "511" => parseStatusCode(511, "NetworkAuthenticationRequired")
          case "598" => parseStatusCode(598, "NetworkReadTimeout")
          case "599" => parseStatusCode(599, "NetworkConnectTimeout")
          case _     => Target.raiseError(s"Unknown HTTP type: ${key}")
        }
    }
  }
}
