package dev.guardrail.generators.Java

import cats.syntax.all._
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.body.BodyDeclaration
import com.github.javaparser.ast.expr._
import dev.guardrail.{ SupportDefinition, Target }
import dev.guardrail.generators.syntax.Java._
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.terms.CollectionsLibTerms
import dev.guardrail.terms.framework._

object DropwizardGenerator {
  def FrameworkInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]): FrameworkTerms[JavaLanguage, Target] =
    new FrameworkInterp
  class FrameworkInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends FrameworkTerms[JavaLanguage, Target] {
    implicit def MonadF = Target.targetInstances

    private lazy val supportDefs: Target[List[SupportDefinition[JavaLanguage]]] = List(
      SerializationHelpers.showerSupportDef
    ).sequence

    def fileType(format: Option[String])   = safeParseType(format.getOrElse("java.io.InputStream"))
    def objectType(format: Option[String]) = safeParseType("com.fasterxml.jackson.databind.JsonNode")

    override def getFrameworkImports(tracing: Boolean): Target[List[ImportDeclaration]] =
      supportDefs.map(_.flatMap(_.imports).distinct)

    def getFrameworkImplicits() =
      Target.pure(None)

    override def getFrameworkDefinitions(tracing: Boolean): Target[List[(Name, List[BodyDeclaration[_ <: BodyDeclaration[_]]])]] =
      supportDefs.map(
        _.map(
          supportDef =>
            (
              supportDef.className,
              supportDef.definition.collect({
                case bd: BodyDeclaration[_] => bd
              })
            )
        )
      )

    def lookupStatusCode(key: String) = {
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
        case _     => Target.raiseUserError(s"Unknown HTTP status code: ${key}")
      }
    }
  }
}
