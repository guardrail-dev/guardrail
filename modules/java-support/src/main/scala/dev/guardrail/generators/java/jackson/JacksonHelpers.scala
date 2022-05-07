package dev.guardrail.generators.java.jackson

import dev.guardrail.Target
import dev.guardrail.core.{ LiteralRawType, ReifiedRawType }
import dev.guardrail.languages.LA
import dev.guardrail.terms.LanguageTerms
import scala.util.Try

/* There's a copy of this file in guardrail-scala-support,
 * modules/scala-support/src/main/scala/dev/guardrail/generators/scala/jackson/JacksonHelpers.scala
 */
object JacksonHelpers {
  def discriminatorExpression[L <: LA](
      discriminatorName: String,
      discriminatorValue: String,
      discriminatorTpe: ReifiedRawType
  )(litBigInteger: String => Target[L#Term], litBigDecimal: String => Target[L#Term], fallback: String => Target[L#Term])(implicit
      Lt: LanguageTerms[L, Target]
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
