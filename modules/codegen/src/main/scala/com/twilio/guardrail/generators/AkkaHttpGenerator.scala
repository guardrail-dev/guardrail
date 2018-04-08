package com.twilio.guardrail
package generators

import _root_.io.swagger.models._
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
      case GetFrameworkImports(tracing) => Target.pure(List(
        q"import akka.http.scaladsl.model._"
      , q"import akka.http.scaladsl.model.headers.RawHeader"
      , q"import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller, FromEntityUnmarshaller}"
      , q"import akka.http.scaladsl.marshalling.{Marshal, Marshaller, Marshalling, ToEntityMarshaller, ToResponseMarshaller}"
      , q"import akka.http.scaladsl.server.Directives._"
      , q"import akka.http.scaladsl.server.{Directive, Directive0, Route}"
      , q"import akka.http.scaladsl.util.FastFuture"
      , q"import akka.stream.Materializer"
      , q"import akka.stream.scaladsl.Source"
      , q"import akka.util.ByteString"
      , q"import cats.data.EitherT"
      , q"import scala.concurrent.{ExecutionContext, Future}"
      , q"import scala.language.implicitConversions"
      ))

      case GetFrameworkImplicits() =>
        val jsonType: Type = t"io.circe.Json"
        val jsonEncoderTypeclass: Type = t"io.circe.Encoder"
        val jsonDecoderTypeclass: Type = t"io.circe.Decoder"
        Target.pure(q"""
          object AkkaHttpImplicits {
            private[this] def pathEscape(s: String): String = Uri.Path.Segment.apply(s, Uri.Path.Empty).toString
            implicit def addShowablePath[T](implicit ev: Show[T]): AddPath[T] = AddPath.build[T](v => pathEscape(ev.show(v)))

            private[this] def argEscape(k: String, v: String): String = Uri.Query.apply((k, v)).toString
            implicit def addShowableArg[T](implicit ev: Show[T]): AddArg[T] = AddArg.build[T](key => v => argEscape(key, ev.show(v)))

            type HttpClient = HttpRequest => Future[HttpResponse]
            type TraceBuilder = String => HttpClient => HttpClient

            implicit final def jsonMarshaller(
                implicit printer: Printer = Printer.noSpaces
            ): ToEntityMarshaller[${jsonType}] =
              Marshaller.withFixedContentType(MediaTypes.${Term.Name("`application/json`")}) { json =>
                HttpEntity(MediaTypes.${Term.Name("`application/json`")}, printer.pretty(json))
              }

            implicit final def jsonEntityMarshaller[A](
                implicit J: ${jsonEncoderTypeclass}[A],
                         printer: Printer = Printer.noSpaces
            ): ToEntityMarshaller[A] =
              jsonMarshaller(printer).compose(J.apply)

            implicit final val jsonUnmarshaller: FromEntityUnmarshaller[${jsonType}] =
              Unmarshaller.byteStringUnmarshaller
                .forContentTypes(MediaTypes.${Term.Name("`application/json`")})
                .map {
                  case ByteString.empty => throw Unmarshaller.NoContentException
                  case data             => jawn.parseByteBuffer(data.asByteBuffer).fold(throw _, identity)
                }

            implicit def jsonEntityUnmarshaller[A](implicit J: ${jsonDecoderTypeclass}[A]): FromEntityUnmarshaller[A] = {
              def decode(json: ${jsonType}) = J.decodeJson(json).fold(throw _, identity)
              jsonUnmarshaller.map(decode)
            }

            implicit val ignoredUnmarshaller: FromEntityUnmarshaller[IgnoredEntity] =
              Unmarshaller.strict(_ => IgnoredEntity.empty)
          }
        """)
    }
  }
}
