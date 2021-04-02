package dev.guardrail.generators.Scala

import cats.Monad
import dev.guardrail.{ Target, UserError }
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.terms.CollectionsLibTerms
import dev.guardrail.terms.framework.FrameworkTerms

import scala.meta._
import scala.util.Try

object DropwizardGenerator {
  def FrameworkInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): FrameworkTerms[ScalaLanguage, Target] =
    new FrameworkInterp
  class FrameworkInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends FrameworkTerms[ScalaLanguage, Target] {
    override def MonadF: Monad[Target] = Target.targetInstances

    override def objectType(format: Option[String]): Target[Type] = Target.pure(t"com.fasterxml.jackson.databind.JsonNode")
    override def fileType(format: Option[String]): Target[Type]   = Target.pure(format.fold[Type](t"java.io.InputStream")(Type.Name.apply))

    override def getFrameworkImports(tracing: Boolean): Target[List[Import]]                      = Target.pure(List.empty)
    override def getFrameworkImplicits(): Target[Option[(Term.Name, Defn.Object)]]                = Target.pure(None)
    override def getFrameworkDefinitions(tracing: Boolean): Target[List[(Term.Name, List[Defn])]] = Target.pure(List.empty)

    // jaxrs has a Status enum, but it is missing a _lot_ of codes,
    // so we'll make our own here and use ints in the generated code
    override def lookupStatusCode(key: String): Target[(Int, Term.Name)] =
      key match {
        case "100" => Target.pure((100, q"Continue"))
        case "101" => Target.pure((101, q"SwitchingProtocols"))
        case "102" => Target.pure((102, q"Processing"))

        case "200" => Target.pure((200, q"OK"))
        case "201" => Target.pure((201, q"Created"))
        case "202" => Target.pure((202, q"Accepted"))
        case "203" => Target.pure((203, q"NonAuthoritativeInformation"))
        case "204" => Target.pure((204, q"NoContent"))
        case "205" => Target.pure((205, q"ResetContent"))
        case "206" => Target.pure((206, q"PartialContent"))
        case "207" => Target.pure((207, q"MultiStatus"))
        case "208" => Target.pure((208, q"AlreadyReported"))
        case "226" => Target.pure((226, q"IMUsed"))

        case "300" => Target.pure((300, q"MultipleChoices"))
        case "301" => Target.pure((301, q"MovedPermanently"))
        case "302" => Target.pure((302, q"Found"))
        case "303" => Target.pure((303, q"SeeOther"))
        case "304" => Target.pure((304, q"NotModified"))
        case "305" => Target.pure((305, q"UseProxy"))
        case "307" => Target.pure((307, q"TemporaryRedirect"))
        case "308" => Target.pure((308, q"PermanentRedirect"))

        case "400" => Target.pure((400, q"BadRequest"))
        case "401" => Target.pure((401, q"Unauthorized"))
        case "402" => Target.pure((402, q"PaymentRequired"))
        case "403" => Target.pure((403, q"Forbidden"))
        case "404" => Target.pure((404, q"NotFound"))
        case "405" => Target.pure((405, q"MethodNotAllowed"))
        case "406" => Target.pure((406, q"NotAcceptable"))
        case "407" => Target.pure((407, q"ProxyAuthenticationRequired"))
        case "408" => Target.pure((408, q"RequestTimeout"))
        case "409" => Target.pure((409, q"Conflict"))
        case "410" => Target.pure((410, q"Gone"))
        case "411" => Target.pure((411, q"LengthRequired"))
        case "412" => Target.pure((412, q"PreconditionFailed"))
        case "413" => Target.pure((413, q"RequestEntityTooLarge"))
        case "414" => Target.pure((414, q"RequestUriTooLong"))
        case "415" => Target.pure((415, q"UnsupportedMediaType"))
        case "416" => Target.pure((416, q"RequestedRangeNotSatisfiable"))
        case "417" => Target.pure((417, q"ExpectationFailed"))
        case "418" => Target.pure((418, q"ImATeapot"))
        case "420" => Target.pure((420, q"EnhanceYourCalm"))
        case "422" => Target.pure((422, q"UnprocessableEntity"))
        case "423" => Target.pure((423, q"Locked"))
        case "424" => Target.pure((424, q"FailedDependency"))
        case "425" => Target.pure((425, q"UnorderedCollection"))
        case "426" => Target.pure((426, q"UpgradeRequired"))
        case "428" => Target.pure((428, q"PreconditionRequired"))
        case "429" => Target.pure((429, q"TooManyRequests"))
        case "431" => Target.pure((431, q"RequestHeaderFieldsTooLarge"))
        case "449" => Target.pure((449, q"RetryWith"))
        case "450" => Target.pure((450, q"BlockedByParentalControls"))
        case "451" => Target.pure((451, q"UnavailableForLegalReasons"))

        case "500" => Target.pure((500, q"InternalServerError"))
        case "501" => Target.pure((501, q"NotImplemented"))
        case "502" => Target.pure((502, q"BadGateway"))
        case "503" => Target.pure((503, q"ServiceUnavailable"))
        case "504" => Target.pure((504, q"GatewayTimeout"))
        case "505" => Target.pure((505, q"HTTPVersionNotSupported"))
        case "506" => Target.pure((506, q"VariantAlsoNegotiates"))
        case "507" => Target.pure((507, q"InsufficientStorage"))
        case "508" => Target.pure((508, q"LoopDetected"))
        case "509" => Target.pure((509, q"BandwidthLimitExceeded"))
        case "510" => Target.pure((510, q"NotExtended"))
        case "511" => Target.pure((511, q"NetworkAuthenticationRequired"))
        case "598" => Target.pure((598, q"NetworkReadTimeout"))
        case "599" => Target.pure((599, q"NetworkConnectTimeout"))

        case custom =>
          val customCode = Try(custom.toInt)
            .filter(code => code >= 100 && code <= 599)
            .map((_, Term.Name(s"StatusCode$custom")))
            .toOption
          Target.fromOption(customCode, UserError(s"'$custom' is not a valid HTTP status code"))
      }
  }
}
