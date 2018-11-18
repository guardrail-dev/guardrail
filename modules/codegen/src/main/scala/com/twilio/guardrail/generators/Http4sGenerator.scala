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
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.terms.framework._
import java.util.Locale
import scala.collection.JavaConverters._
import scala.meta._

object Http4sGenerator {
  object FrameworkInterp extends FunctionK[FrameworkTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: FrameworkTerm[ScalaLanguage, T]): Target[T] = term match {
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
            q"import org.http4s.EntityEncoder._",
            q"import org.http4s.EntityDecoder._",
            q"import fs2.Stream",
            q"import io.circe.Json",
            q"import scala.language.implicitConversions"
          )
        )

      case GetFrameworkImplicits() =>
        Target.pure(q"""
          object Http4sImplicits {
            import scala.util.Try
            private[this] def pathEscape(s: String): String = Path(s).toString.drop(1)
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
        """)

      case GetGeneratorSettings() =>
        Target.getGeneratorSettings
    }
  }
}
