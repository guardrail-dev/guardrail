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

object Http4sGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm, Target] {
    def apply[T](term: FrameworkTerm[T]): Target[T] = term match {
      case GetFrameworkImports(tracing) =>
        Target.pure(
          List(
            q"import cats.data.EitherT",
            q"import cats.implicits._",
            q"import cats.effect.IO",
            q"import cats.effect.Effect",
            q"import org.http4s._",
            q"import org.http4s.circe._",
            q"import org.http4s.client._",
            q"import org.http4s.client.blaze._",
            q"import org.http4s.dsl.io.Path",
            q"import org.http4s.multipart._",
            q"import scala.language.implicitConversions"
          ))

      case GetFrameworkImplicits() =>
        val jsonType: Type = t"io.circe.Json"
        val jsonEncoderTypeclass: Type = t"io.circe.Encoder"
        val jsonDecoderTypeclass: Type = t"io.circe.Decoder"
        Target.pure(q"""
          object Http4sImplicits {
            import scala.util.Try
            private[this] def pathEscape(s: String): String = Path(s).toString.drop(1)
            implicit def addShowablePath[T](implicit ev: Show[T]): AddPath[T] = AddPath.build[T](v => pathEscape(ev.show(v)))

            private[this] def argEscape(k: String, v: String): String = Query.apply((k, Some(v))).toString
            implicit def addShowableArg[T](implicit ev: Show[T]): AddArg[T] = AddArg.build[T](key => v => argEscape(key, ev.show(v)))

            type TraceBuilder[F[_]] = String => org.http4s.client.Client[F] => org.http4s.client.Client[F]

            implicit def entityEncoder[F[_]: Effect, T: ${jsonEncoderTypeclass}]: EntityEncoder[F, T] = jsonEncoderOf[F, T]
            implicit def entityDecoder[F[_]: Effect, T: ${jsonDecoderTypeclass}]: EntityDecoder[F, T] = jsonOf[F, T]
            implicit def entityIgnoredEntityDecoder[F[_]: Effect]: EntityDecoder[F, IgnoredEntity] = EntityDecoder[F, Unit].map { _ => IgnoredEntity.empty }
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
        """)
    }
  }
}
