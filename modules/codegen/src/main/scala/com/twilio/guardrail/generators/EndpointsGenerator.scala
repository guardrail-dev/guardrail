package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import com.twilio.guardrail.terms.framework._
import scala.meta._
import com.twilio.guardrail.languages.ScalaLanguage

object EndpointsGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: FrameworkTerm[ScalaLanguage, T]): Target[T] = term match {
      case FileType(format)   => Target.pure(format.fold[Type](t"org.scalajs.dom.raw.File")(Type.Name(_)))
      case ObjectType(format) => Target.pure(t"io.circe.Json")
      case GetFrameworkImports(tracing) =>
        Target.pure(
          List(
            q"import _root_.faithful.{ Future, Promise }",
            q"import cats.{Functor, Id}",
            q"import cats.data.EitherT",
            q"import cats.implicits._",
            q"import endpoints.algebra",
            q"import endpoints.algebra.Documentation",
            q"import endpoints.xhr",
            q"import io.circe.{parser, Decoder => CirceDecoder, Encoder => CirceEncoder}",
            q"import org.scalajs.dom.raw.XMLHttpRequest"
          )
        )
      case GetFrameworkImplicits() => Target.pure(Some((q"EndpointsImplicits", q"""
        object EndpointsImplicits {
          case class UnknownStatusException(xhr: XMLHttpRequest) extends Exception
          implicit class FaithfulFutureExtensions[T](value: Future[T]) {
            def transformWith[U](success: Function1[T, Future[U]], error: Function1[Throwable, Future[U]]): Future[U] = {
              val p = new Promise[U]()
              val _ = value.apply(x => success(x).apply(p.success _, p.failure _), e => error(e).apply(p.success _, p.failure _))
              p.future
            }
          }
          trait AddPathSegments extends algebra.Urls {
            implicit def addShowableArg[T](implicit ev: Show[T]): AddArg[T]
            implicit def addShowablePath[T](implicit ev: Show[T]): AddPath[T]
            def showQs[A](name: String, docs: Documentation = None)(implicit ev: AddArg[A]): QueryString[A]
            def showStaticQs[A](name: String, value: A, docs: Documentation = None)(implicit ev: AddArg[A]): QueryString[Unit]
            def showSegment[A](name: String, docs: Documentation)(implicit ev: AddPath[A]): Path[A]
            def pathRoot: Path[(String, Option[String])]
          }
          trait FormDataEncoder[T] { def apply: T => String }
          object FormDataEncoder { def apply[T](implicit ev: FormDataEncoder[T]): FormDataEncoder[T] = ev }
          trait FormData extends algebra.Endpoints {
            type TraceBuilder = String => RequestHeaders[String]
            type FileType
            implicit def stringPairEncoder: FormDataEncoder[List[(String, String)]]
            def formDataRequest[A]()(implicit ev: FormDataEncoder[A]): RequestEntity[A]
            def multipartFormDataRequest[A](): RequestEntity[List[(String, Either[String, FileType])]]
            def textPlainRequest: RequestEntity[String]
            def showHeader[A](name: String, docs: Documentation)(implicit ev: Show[A]): RequestHeaders[A]
            def showOptHeader[A](name: String, docs: Documentation)(implicit ev: Show[A]): RequestHeaders[Option[A]]
            def tracerHeader: RequestHeaders[Map[String, String]]
          }
          trait XhrAddPathSegments extends AddPathSegments with xhr.Urls {
            import scala.scalajs.js.URIUtils
            private[this] def argEscape(k: String, v: String): String = URIUtils.encodeURIComponent(k) ++ "=" ++ URIUtils.encodeURIComponent(v)
            implicit def addShowableArg[T](implicit ev: Show[T]): AddArg[T] = AddArg.build[T](key => v => argEscape(key, ev.show(v)))
            implicit def addShowablePath[T](implicit ev: Show[T]): AddPath[T] = AddPath.build[T](v => URIUtils.encodeURIComponent(ev.show(v)))
            def showQs[A](name: String, docs: Documentation = None)(implicit ev: AddArg[A]): QueryString[A] = a => ev.addArg(name, a)
            def showStaticQs[A](name: String, value: A, docs: Documentation = None)(implicit ev: AddArg[A]): QueryString[Unit] = new QueryString[Unit] { def encode(unit: Unit): String = ev.addArg(name, value) }
            def showSegment[A](name: String, docs: Documentation)(implicit ev: AddPath[A]): Path[A] = a => ev.addPath(a)
            def pathRoot: Path[(String, Option[String])] = { case (host, basePath) =>
              basePath.fold(host) { bp => host ++ bp }
            }
          }
          trait XhrFormData extends FormData with xhr.Endpoints {
            import scala.scalajs.js.URIUtils
            type FileType = org.scalajs.dom.raw.File
            private[this] def argEscape(k: String, v: String): String = URIUtils.encodeURIComponent(k) ++ "=" ++ URIUtils.encodeURIComponent(v)
            implicit val stringPairEncoder: FormDataEncoder[List[(String, String)]] = new FormDataEncoder[List[(String, String)]] {
              private[this] def encode(k: String, v: String): String = URIUtils.encodeURIComponent(k) + "=" + URIUtils.encodeURIComponent(v)
              def apply = {
                case (k, v) :: xs =>
                  xs.foldLeft(encode(k, v))({
                    case (acc, (k, v)) =>
                      acc + "&" + encode(k, v)
                  })
                case Nil =>
                  ""
              }
            }
            def formDataRequest[A]()(implicit ev: FormDataEncoder[A]): RequestEntity[A] = (a: A, xhr: XMLHttpRequest) => {
              xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded")
              ev.apply(a)
            }
            def multipartFormDataRequest[A](): RequestEntity[List[(String, Either[String, FileType])]] = { (a, xhr) =>
              val formDataAppender = new org.scalajs.dom.raw.FormData()
              a.foreach({ case (k, v) =>
                v.fold(formDataAppender.append(k, _), formDataAppender.append(k, _))
              })
              formDataAppender
            }
            def textPlainRequest: RequestEntity[String] = (a: String, xhr: XMLHttpRequest) => {
              xhr.setRequestHeader("Content-type", "text/plain")
              a
            }
            def showHeader[A](name: String, docs: Documentation)(implicit ev: Show[A]): RequestHeaders[A] = (value, xhr) => xhr.setRequestHeader(name, ev.show(value))
            def showOptHeader[A](name: String, docs: Documentation)(implicit ev: Show[A]): RequestHeaders[Option[A]] = (value, xhr) => value.foreach(showHeader(name, docs).apply(_, xhr))
            def tracerHeader: RequestHeaders[Map[String, String]] = { (value, xhr) =>
              value.foreach({ case (k, v) => xhr.setRequestHeader(k, v) })
            }
          }
        }
      """)))

      case GetFrameworkDefinitions(tracing) =>
        Target.pure(List.empty)

      case LookupStatusCode(key) =>
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
          case _     => Target.raiseError(s"Unknown HTTP type: ${key}")
        }
    }
  }
}
