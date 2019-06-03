package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import com.twilio.guardrail.terms.framework._
import scala.meta._
import com.twilio.guardrail.languages.ScalaLanguage

object AkkaHttpGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: FrameworkTerm[ScalaLanguage, T]): Target[T] = term match {
      case FileType(format)   => Target.pure(format.fold[Type](t"akka.http.scaladsl.model.BodyPartEntity")(Type.Name(_)))
      case ObjectType(format) => Target.pure(t"io.circe.Json")

      case GetFrameworkImports(tracing) =>
        Target.pure(
          List(
            q"import akka.http.scaladsl.model._",
            q"import akka.http.scaladsl.model.headers.RawHeader",
            q"import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller, FromEntityUnmarshaller, FromRequestUnmarshaller, FromStringUnmarshaller}",
            q"import akka.http.scaladsl.marshalling.{Marshal, Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshaller}",
            q"import akka.http.scaladsl.server.Directives._",
            q"import akka.http.scaladsl.server.{Directive, Directive0, Directive1, ExceptionHandler, MalformedHeaderRejection, MissingFormFieldRejection, Rejection, Route}",
            q"import akka.http.scaladsl.util.FastFuture",
            q"import akka.stream.{IOResult, Materializer}",
            q"import akka.stream.scaladsl.{FileIO, Keep, Sink, Source}",
            q"import akka.util.ByteString",
            q"import io.circe.Decoder",
            q"import cats.{Functor, Id}",
            q"import cats.data.EitherT",
            q"import cats.implicits._",
            q"import scala.concurrent.{ExecutionContext, Future}",
            q"import scala.language.higherKinds",
            q"import scala.language.implicitConversions",
            q"import java.io.File",
            q"import java.security.MessageDigest",
            q"import java.util.concurrent.atomic.AtomicReference",
            q"import scala.util.{Failure, Success}"
          )
        )

      case GetFrameworkImplicits() => {
        val jsonEncoderTypeclass: Type = t"io.circe.Encoder"
        val jsonDecoderTypeclass: Type = t"io.circe.Decoder"
        val jsonType: Type             = t"io.circe.Json"
        val defn                       = q"""
          object AkkaHttpImplicits {
            private[this] def pathEscape(s: String): String = Uri.Path.Segment.apply(s, Uri.Path.Empty).toString
            implicit def addShowablePath[T](implicit ev: Show[T]): AddPath[T] = AddPath.build[T](v => pathEscape(ev.show(v)))

            private[this] def argEscape(k: String, v: String): String = Uri.Query.apply((k, v)).toString
            implicit def addShowableArg[T](implicit ev: Show[T]): AddArg[T] = AddArg.build[T](key => v => argEscape(key, ev.show(v)))

            type HttpClient = HttpRequest => Future[HttpResponse]
            type TraceBuilder = String => HttpClient => HttpClient

            class TextPlain(val value: String)
            object TextPlain {
              def apply(value: String): TextPlain = new TextPlain(value)
              implicit final def textTEM: ToEntityMarshaller[TextPlain] =
                Marshaller.withFixedContentType(ContentTypes.`text/plain(UTF-8)`) { text =>
                  HttpEntity(ContentTypes.`text/plain(UTF-8)`, text.value)
                }
            }

            sealed trait IgnoredEntity
            object IgnoredEntity {
              val empty: IgnoredEntity = new IgnoredEntity {}
            }

            implicit final def jsonMarshaller(
                implicit printer: Printer = Printer.noSpaces
            ): ToEntityMarshaller[${jsonType}] =
              Marshaller.withFixedContentType(MediaTypes.`application/json`) { json =>
                HttpEntity(MediaTypes.`application/json`, printer.pretty(json))
              }

            implicit final def jsonEntityMarshaller[A](
                implicit J: ${jsonEncoderTypeclass}[A],
                         printer: Printer = Printer.noSpaces
            ): ToEntityMarshaller[A] =
              jsonMarshaller(printer).compose(J.apply)

            final val stringyJsonEntityUnmarshaller: FromEntityUnmarshaller[${jsonType}] =
              Unmarshaller.byteStringUnmarshaller
                .forContentTypes(MediaTypes.`text/plain`)
                .map({
                  case ByteString.empty =>
                    throw Unmarshaller.NoContentException
                  case data =>
                    Json.fromString(data.decodeString("utf-8"))
                })

            implicit final val structuredJsonEntityUnmarshaller: FromEntityUnmarshaller[${jsonType}] =
              Unmarshaller.byteStringUnmarshaller
                .forContentTypes(MediaTypes.`application/json`)
                .flatMapWithInput { (httpEntity, byteString) =>
                  if (byteString.isEmpty) {
                    FastFuture.failed(Unmarshaller.NoContentException)
                  } else {
                    val parseResult = Unmarshaller.bestUnmarshallingCharsetFor(httpEntity) match {
                      case HttpCharsets.`UTF-8` => jawn.parse(byteString.utf8String)
                      case otherCharset => jawn.parse(byteString.decodeString(otherCharset.nioCharset.name))
                    }
                    parseResult.fold(FastFuture.failed, FastFuture.successful)
                  }
                }

            implicit def jsonEntityUnmarshaller[A](implicit J: ${jsonDecoderTypeclass}[A]): FromEntityUnmarshaller[A] = {
              Unmarshaller.firstOf(structuredJsonEntityUnmarshaller, stringyJsonEntityUnmarshaller)
                .flatMap(_ => _ => json => J.decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
            }

            final val jsonStringUnmarshaller: FromStringUnmarshaller[${jsonType}] = Unmarshaller.strict {
              case "" =>
                throw Unmarshaller.NoContentException
              case data =>
                jawn.parse(data).getOrElse(Json.fromString(data))
            }

            implicit def jsonDecoderUnmarshaller[A](implicit J: ${jsonDecoderTypeclass}[A]): FromStringUnmarshaller[A] = {
              jsonStringUnmarshaller
                .flatMap(_ => _ => json => J.decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
            }

            implicit val ignoredUnmarshaller: FromEntityUnmarshaller[IgnoredEntity] =
              Unmarshaller.strict(_ => IgnoredEntity.empty)

            implicit def MFDBPviaFSU[T](implicit ev: Unmarshaller[BodyPartEntity, T]): Unmarshaller[Multipart.FormData.BodyPart, T] = Unmarshaller.withMaterializer { implicit executionContext => implicit mat => entity =>
              ev.apply(entity.entity)
            }

            implicit def BPEviaFSU[T](implicit ev: Unmarshaller[String, T]): Unmarshaller[BodyPartEntity, T] = Unmarshaller.withMaterializer { implicit executionContext => implicit mat => entity =>
              entity.dataBytes
                .runWith(Sink.fold(ByteString.empty)((accum, bs) => accum.concat(bs)))
                .map(_.decodeString(java.nio.charset.StandardCharsets.UTF_8))
                .flatMap(ev.apply(_))
            }

            def AccumulatingUnmarshaller[T, U, V](accumulator: AtomicReference[List[V]], ev: Unmarshaller[T, U])(acc: U => V)(implicit mat: Materializer): Unmarshaller[T, U] = {
              ev.map { value =>
                accumulator.updateAndGet(x => (acc(value) :: x))
                value
              }
            }

            def SafeUnmarshaller[T, U](ev: Unmarshaller[T, U])(implicit mat: Materializer): Unmarshaller[T, Either[Throwable, U]] = Unmarshaller { implicit executionContext => entity =>
              ev.apply(entity).map[Either[Throwable, U]](Right(_)).recover({ case t => Left(t) })
            }

            def StaticUnmarshaller[T](value: T)(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, T] = Unmarshaller { _ => part =>
              part.entity.discardBytes()
              Future.successful[T](value)
            }

            implicit def UnitUnmarshaller(implicit mat: Materializer): Unmarshaller[Multipart.FormData.BodyPart, Unit] = StaticUnmarshaller(())
          }
        """
        Target.pure(Some((q"AkkaHttpImplicits", defn)))
      }

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
