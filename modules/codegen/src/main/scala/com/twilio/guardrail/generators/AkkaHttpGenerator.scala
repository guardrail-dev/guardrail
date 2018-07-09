package com.twilio.guardrail
package generators

import io.swagger.models._
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.instances.all._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import com.twilio.guardrail.extract.ScalaPackage
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.terms.framework._
import java.util.Locale
import scala.collection.JavaConverters._
import scala.meta._

object AkkaHttpGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm, Target] {
    def apply[T](term: FrameworkTerm[T]): Target[T] = term match {
      case GetFrameworkImports(tracing) =>
        Target.pure(
          List(
            q"import akka.http.scaladsl.model._",
            q"import akka.http.scaladsl.model.headers.RawHeader",
            q"import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller, FromEntityUnmarshaller, FromStringUnmarshaller}",
            q"import akka.http.scaladsl.marshalling.{Marshal, Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshaller}",
            q"import akka.http.scaladsl.server.Directives._",
            q"import akka.http.scaladsl.server.{Directive, Directive0, Directive1, ExceptionHandler, MissingFormFieldRejection, Rejection, Route}",
            q"import akka.http.scaladsl.util.FastFuture",
            q"import akka.stream.{IOResult, Materializer}",
            q"import akka.stream.scaladsl.{FileIO, Keep, Sink, Source}",
            q"import akka.util.ByteString",
            q"import cats.{Functor, Id}",
            q"import cats.data.EitherT",
            q"import cats.implicits._",
            q"import scala.concurrent.{ExecutionContext, Future}",
            q"import scala.language.implicitConversions",
            q"import java.io.File",
            q"import java.security.MessageDigest",
            q"import java.util.concurrent.atomic.AtomicReference",
            q"import scala.util.{Failure, Success}"
          )
        )

      case GetFrameworkImplicits() =>
        Target.getGeneratorSettings.map { implicit gs =>
          val jsonEncoderTypeclass: Type = t"io.circe.Encoder"
          val jsonDecoderTypeclass: Type = t"io.circe.Decoder"
          q"""
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

            implicit final def jsonMarshaller(
                implicit printer: Printer = Printer.noSpaces
            ): ToEntityMarshaller[${gs.jsonType}] =
              Marshaller.withFixedContentType(MediaTypes.`application/json`) { json =>
                HttpEntity(MediaTypes.`application/json`, printer.pretty(json))
              }

            implicit final def jsonEntityMarshaller[A](
                implicit J: ${jsonEncoderTypeclass}[A],
                         printer: Printer = Printer.noSpaces
            ): ToEntityMarshaller[A] =
              jsonMarshaller(printer).compose(J.apply)

            final val stringyJsonEntityUnmarshaller: FromEntityUnmarshaller[${gs.jsonType}] =
              Unmarshaller.byteStringUnmarshaller
                .forContentTypes(MediaTypes.`text/plain`)
                .map({
                  case ByteString.empty =>
                    throw Unmarshaller.NoContentException
                  case data =>
                    Json.fromString(data.decodeString("utf-8"))
                })

            implicit final val structuredJsonEntityUnmarshaller: FromEntityUnmarshaller[${gs.jsonType}] =
              Unmarshaller.byteStringUnmarshaller
                .forContentTypes(MediaTypes.`application/json`)
                .flatMapWithInput { (httpEntity, byteString) =>
                  val parseResult = Unmarshaller.bestUnmarshallingCharsetFor(httpEntity) match {
                    case HttpCharsets.`UTF-8` => jawn.parse(byteString.utf8String)
                    case otherCharset => jawn.parse(byteString.decodeString(otherCharset.nioCharset.name))
                  }
                  parseResult.fold(_ => FastFuture.failed(Unmarshaller.NoContentException), FastFuture.successful)
                }

            implicit def jsonEntityUnmarshaller[A](implicit J: ${jsonDecoderTypeclass}[A]): FromEntityUnmarshaller[A] = {
              Unmarshaller.firstOf(structuredJsonEntityUnmarshaller, stringyJsonEntityUnmarshaller)
                .flatMap(_ => _ => json => J.decodeJson(json).fold(_ => FastFuture.failed(Unmarshaller.NoContentException), FastFuture.successful))
            }

            final val jsonStringUnmarshaller: FromStringUnmarshaller[${gs.jsonType}] = Unmarshaller.strict {
              case "" =>
                throw Unmarshaller.NoContentException
              case data =>
                jawn.parse(data).getOrElse(Json.fromString(data))
            }

            def jsonDecoderUnmarshaller[A](implicit J: ${jsonDecoderTypeclass}[A]): FromStringUnmarshaller[A] = {
              def decode(json: ${gs.jsonType}) = J.decodeJson(json).valueOr(throw _)
              jsonStringUnmarshaller.map(decode _)
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
                accumulator.updateAndGet(acc(value) :: _)
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
        }

      case GetGeneratorSettings() =>
        Target.getGeneratorSettings
    }
  }
}
