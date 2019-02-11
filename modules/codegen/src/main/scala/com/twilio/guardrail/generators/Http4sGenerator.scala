package com.twilio.guardrail
package generators

import _root_.io.swagger.v3.oas.models._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.extract.ScalaPackage
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms.framework._
import java.util.Locale
import scala.collection.JavaConverters._
import scala.meta._

object Http4sGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: FrameworkTerm[ScalaLanguage, T]): Target[T] = term match {
      case FileType(format) =>
        Target.pure(format.fold[Type](t"java.io.File")(Type.Name(_)))

      case ObjectType(format) => Target.pure(t"io.circe.Json")

      case GetFrameworkImports(tracing) =>
        Target.pure(
          List(
            q"import cats.data.EitherT",
            q"import cats.implicits._",
            q"import cats.effect.IO",
            q"import cats.effect.Effect",
            q"import org.http4s.{Status => _, _}",
            q"import org.http4s.circe._",
            q"import org.http4s.client.{Client => Http4sClient}",
            q"import org.http4s.client.blaze._",
            q"import org.http4s.client.UnexpectedStatus",
            q"import org.http4s.dsl.io.Path",
            q"import org.http4s.multipart._",
            q"import org.http4s.headers._",
            q"import org.http4s.implicits._",
            q"import org.http4s.EntityEncoder._",
            q"import org.http4s.EntityDecoder._",
            q"import fs2.Stream",
            q"import io.circe.Json",
            q"import scala.language.higherKinds",
            q"import scala.language.implicitConversions"
          )
        )

      case GetFrameworkImplicits() =>
        Target.pure({
          val defn = q"""
          object Http4sImplicits {
            import scala.util.Try
            private[this] def pathEscape(s: String): String = Path(s, "").toString.init.tail
            implicit def addShowablePath[T](implicit ev: Show[T]): AddPath[T] = AddPath.build[T](v => pathEscape(ev.show(v)))

            private[this] def argEscape(k: String, v: String): String = Query.apply((k, Some(v))).toString
            implicit def addShowableArg[T](implicit ev: Show[T]): AddArg[T] = AddArg.build[T](key => v => argEscape(key, ev.show(v)))

            type TraceBuilder[F[_]] = String => org.http4s.client.Client[F] => org.http4s.client.Client[F]

            implicit def emptyEntityEncoder[F[_]: Effect]: EntityEncoder[F, EntityBody[Nothing]] = EntityEncoder.emptyEncoder

            object DoubleNumber {
              def unapply(value: String): Option[Double] = Try(value.toDouble).toOption
            }

            object BigDecimalNumber {
              def unapply(value: String): Option[BigDecimal] = Try(BigDecimal(value)).toOption
            }

            object BigIntNumber {
              def unapply(value: String): Option[BigInt] = Try(BigInt(value)).toOption
            }
          }
        """
          (q"Http4sImplicits", defn)
        })

      case LookupStatusCode(key) =>
        key match {
          case "100" => Target.pure((100, q"Continue"))
          case "101" => Target.pure((101, q"SwitchingProtocols"))
          case "102" => Target.pure((102, q"Processing"))

          case "200" => Target.pure((200, q"Ok"))
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
          case "413" => Target.pure((413, q"PayloadTooLarge"))
          case "414" => Target.pure((414, q"UriTooLong"))
          case "415" => Target.pure((415, q"UnsupportedMediaType"))
          case "416" => Target.pure((416, q"RangeNotSatisfiable"))
          case "417" => Target.pure((417, q"ExpectationFailed"))
          case "422" => Target.pure((422, q"UnprocessableEntity"))
          case "423" => Target.pure((423, q"Locked"))
          case "424" => Target.pure((424, q"FailedDependency"))
          case "426" => Target.pure((426, q"UpgradeRequired"))
          case "428" => Target.pure((428, q"PreconditionRequired"))
          case "429" => Target.pure((429, q"TooManyRequests"))
          case "431" => Target.pure((431, q"RequestHeaderFieldsTooLarge"))
          case "451" => Target.pure((451, q"UnavailableForLegalReasons"))

          case "500" => Target.pure((500, q"InternalServerError"))
          case "501" => Target.pure((501, q"NotImplemented"))
          case "502" => Target.pure((502, q"BadGateway"))
          case "503" => Target.pure((503, q"ServiceUnavailable"))
          case "504" => Target.pure((504, q"GatewayTimeout"))
          case "505" => Target.pure((505, q"HttpVersionNotSupported"))
          case "506" => Target.pure((506, q"VariantAlsoNegotiates"))
          case "507" => Target.pure((507, q"InsufficientStorage"))
          case "508" => Target.pure((508, q"LoopDetected"))
          case "510" => Target.pure((510, q"NotExtended"))
          case "511" => Target.pure((511, q"NetworkAuthenticationRequired"))
          case _     => Target.raiseError(s"Unknown HTTP type: ${key}")
        }

    }
  }
}
