package dev.guardrail.generators.helpers

import dev.guardrail.Target
import dev.guardrail.languages.LA
import dev.guardrail.terms.LanguageTerms
import scala.util.Try

object JacksonHelpers {
  def discriminatorExpression[L <: LA](
      discriminatorName: String,
      discriminatorValue: String,
      discriminatorTpe: Option[String],
      discriminatorFmt: Option[String]
  )(litBigInteger: String => Target[L#Term], litBigDecimal: String => Target[L#Term], fallback: String => Target[L#Term])(
      implicit Lt: LanguageTerms[L, Target]
  ): Target[L#Term] = {
    import Lt._

    def parseLiteral[T](parser: String => T, friendlyName: String): Target[T] =
      Try(parser(discriminatorValue)).fold(
        t => Target.raiseUserError[T](s"Unable to parse '$discriminatorValue' as '$friendlyName': ${t.getMessage}"),
        Target.pure[T]
      )
    def errorUnsupported(tpe: String, fmt: String): Target[L#Term] =
      Target.raiseUserError[L#Term](s"Unsupported discriminator type '$tpe' with format '$fmt' for property '$discriminatorName'")

    discriminatorTpe match {
      case Some(tpe @ "string") =>
        discriminatorFmt match {
          case Some("date") | Some("date-time") | Some("byte") | Some("binary") => errorUnsupported(tpe, discriminatorFmt.getOrElse("(none)"))
          case _                                                                => litString(discriminatorValue)
        }
      case Some(tpe @ "boolean") =>
        parseLiteral(_.toBoolean, tpe).flatMap(litBoolean)
      case Some(tpe @ "integer") =>
        discriminatorFmt match {
          case Some(fmt @ "int32") => parseLiteral(_.toInt, fmt).flatMap(litInt)
          case Some(fmt @ "int64") => parseLiteral(_.toLong, fmt).flatMap(litLong)
          case Some(fmt)           => errorUnsupported(tpe, fmt)
          case None                => parseLiteral(BigInt(_).toString, "BigInteger").flatMap(litBigInteger)
        }
      case Some(tpe @ "number") =>
        discriminatorFmt match {
          case Some(fmt @ "float")  => parseLiteral(_.toFloat, fmt).flatMap(litFloat)
          case Some(fmt @ "double") => parseLiteral(_.toDouble, fmt).flatMap(litDouble)
          case Some(fmt)            => errorUnsupported(tpe, fmt)
          case None                 => parseLiteral(BigDecimal(_).toString, "BigDecimal").flatMap(litBigDecimal)
        }
      case Some(tpe) => errorUnsupported(tpe, discriminatorFmt.getOrElse("(none)"))
      case None      => fallback(discriminatorValue)
    }
  }
}
