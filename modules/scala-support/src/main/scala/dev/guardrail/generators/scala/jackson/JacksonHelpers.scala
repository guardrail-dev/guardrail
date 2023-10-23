package dev.guardrail.generators.scala.jackson

import scala.util.Try
import scala.meta.Term

import dev.guardrail.Target
import dev.guardrail.core.{ LiteralRawType, ReifiedRawType }
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.terms.LanguageTerms

/* There's a copy of this file in guardrail-java-support,
 * modules/java-support/src/main/scala/dev/guardrail/generators/java/jackson/JacksonHelpers.scala
 */
object JacksonHelpers {
  def discriminatorExpression(
      discriminatorName: String,
      discriminatorValue: String,
      discriminatorTpe: ReifiedRawType
  )(litBigInteger: String => Target[Term], litBigDecimal: String => Target[Term], fallback: String => Target[Term])(implicit
      Lt: LanguageTerms[ScalaLanguage, Target]
  ): Target[Term] = {
    import Lt._

    def parseLiteral[T](parser: String => T, friendlyName: String): Target[T] =
      Try(parser(discriminatorValue)).fold(
        t => Target.raiseUserError[T](s"Unable to parse '$discriminatorValue' as '$friendlyName': ${t.getMessage}"),
        Target.pure[T]
      )
    def errorUnsupported(tpe: String, fmt: String): Target[Term] =
      Target.raiseUserError[Term](s"Unsupported discriminator type '$tpe' with format '$fmt' for property '$discriminatorName'")

    discriminatorTpe match {
      case LiteralRawType(Some(tpe @ "string"), fmt) =>
        fmt match {
          case Some("date") | Some("date-time") | Some("byte") | Some("binary") => errorUnsupported(tpe, fmt.getOrElse("(none)"))
          case _                                                                => litString(discriminatorValue)
        }
      case LiteralRawType(Some(tpe @ "boolean"), _) =>
        parseLiteral(_.toBoolean, tpe).flatMap(litBoolean)
      case LiteralRawType(Some(tpe @ "integer"), fmt) =>
        fmt match {
          case Some(fmt @ "int32") => parseLiteral(_.toInt, fmt).flatMap(litInt)
          case Some(fmt @ "int64") => parseLiteral(_.toLong, fmt).flatMap(litLong)
          case Some(fmt)           => errorUnsupported(tpe, fmt)
          case None                => parseLiteral(BigInt(_).toString, "BigInteger").flatMap(litBigInteger)
        }
      case LiteralRawType(Some(tpe @ "number"), fmt) =>
        fmt match {
          case Some(fmt @ "float")  => parseLiteral(_.toFloat, fmt).flatMap(litFloat)
          case Some(fmt @ "double") => parseLiteral(_.toDouble, fmt).flatMap(litDouble)
          case Some(fmt)            => errorUnsupported(tpe, fmt)
          case None                 => parseLiteral(BigDecimal(_).toString, "BigDecimal").flatMap(litBigDecimal)
        }
      case LiteralRawType(Some(tpe), fmt) => errorUnsupported(tpe, fmt.getOrElse("(none)"))
      case _                              => fallback(discriminatorValue)
    }
  }
}
