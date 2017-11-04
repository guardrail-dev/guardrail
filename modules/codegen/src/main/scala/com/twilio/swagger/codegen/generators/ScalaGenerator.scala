package com.twilio.swagger.codegen
package generators

import cats.syntax.either._
import cats.~>
import com.twilio.swagger.codegen.terms._
import scala.meta._

object ScalaGenerator {
  object ScalaInterp extends (ScalaTerm ~> Target) {
    def apply[T](term: ScalaTerm[T]): Target[T] = term match {
      case RenderImplicits(pkgName, frameworkImports, jsonImports, customImports) =>
        val pkg: Term.Ref = pkgName.map(Term.Name.apply _).reduceLeft(Term.Select.apply _)
        val jsonType: Type = t"io.circe.Json"
        val jsonEncoderTypeclass: Type = t"io.circe.Encoder"
        val jsonDecoderTypeclass: Type = t"io.circe.Decoder"
        Target.pure(
          source"""
            package ${pkg}

            ..${customImports}

            ..${jsonImports}

            ..${frameworkImports}

            import cats.implicits._
            import cats.data.EitherT

            import scala.concurrent.Future

            object Implicits {
              private[this] def pathEscape(s: String): String = Uri.Path.Segment.apply(s, Uri.Path.Empty).toString
              private[this] def argEscape(k: String, v: String): String = Uri.Query.apply((k, v)).toString

              abstract class AddArg[T] {
                def addArg(key: String, v: T): String
              }

              object AddArg {
                def build[T](f: String => T => String): AddArg[T] = new AddArg[T] {
                  def addArg(key: String, v: T): String = f(key)(v)
                }

                implicit def addShowableArg[T](implicit ev: Show[T]): AddArg[T] = build[T](key => v => argEscape(key, ev.show(v)))
                implicit def addArgSeq[T](implicit ev: AddArg[T]): AddArg[List[T]] = build[List[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgIterable[T](implicit ev: AddArg[T]): AddArg[Iterable[T]] = build[Iterable[T]](key => vs => vs.map(v => ev.addArg(key, v)).mkString("&"))
                implicit def addArgOption[T](implicit ev: AddArg[T]): AddArg[Option[T]] = build[Option[T]](key => v => v.map(ev.addArg(key, _)).getOrElse(""))
              }

              abstract class AddPath[T] {
                def addPath(v: T): String
              }

              object AddPath {
                def build[T](f: T => String): AddPath[T] = new AddPath[T] {
                  def addPath(v: T): String = f(v)
                }

                implicit def addShowablePath[T](implicit ev: Show[T]): AddPath[T] = build[T](v => pathEscape(ev.show(v)))
              }

              abstract class Show[T] {
                def show(v: T): String
              }

              object Show {
                def build[T](f: T => String): Show[T] = new Show[T] {
                  def show(v: T): String = f(v)
                }

                implicit val showString = build[String](identity)
                implicit val showInt = build[Int](_.toString)
                implicit val showLong = build[Long](_.toString)
                implicit val showBigInt = build[BigInt](_.toString)
                implicit val showBigDecimal = build[BigDecimal](_.toString)
                implicit val showBoolean = build[Boolean](_.toString)
                implicit val showOffsetDateTime = build[java.time.OffsetDateTime](_.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME))
                implicit val showAnyVal = build[AnyVal](_.toString)
              }

              object Formatter {
                def show[T](value: T)(implicit ev: Show[T]): String = {
                  ev.show(value)
                }

                def addArg[T](key: String, value: T)(implicit ev: AddArg[T]): String = {
                  s"&$${ev.addArg(key, value)}"
                }

                def addPath[T](value: T)(implicit ev: AddPath[T]): String = {
                  ev.addPath(value)
                }
              }

              type TraceBuilder[E, T] = String => ((HttpRequest => HttpRequest) => EitherT[Future, E, T]) => EitherT[Future, E, T]

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

              sealed trait IgnoredEntity
              object IgnoredEntity {
                val empty: IgnoredEntity = new IgnoredEntity {}
              }
              implicit val ignoredUnmarshaller: FromEntityUnmarshaller[IgnoredEntity] =
                Unmarshaller.strict(_ => IgnoredEntity.empty)
            }
          """
        )
    }
  }
}
