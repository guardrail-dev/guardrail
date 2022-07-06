package dev.guardrail.generators.scala.circe

import _root_.io.swagger.v3.oas.models.media.Schema
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.generators.scala.{ CirceRefinedModelGenerator, ScalaLanguage }
import dev.guardrail.generators.spi.{ ModuleLoadResult, ProtocolGeneratorLoader }
import dev.guardrail.terms.ProtocolTerms

import scala.meta._
import scala.reflect.runtime.universe.typeTag
import scala.util.Try

class CirceRefinedProtocolGeneratorLoader extends ProtocolGeneratorLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  val apply =
    ModuleLoadResult.forProduct1(ProtocolGeneratorLoader.label -> Seq(CirceRefinedModelGenerator.mapping))(circeVersion =>
      CirceRefinedProtocolGenerator(circeVersion)
    )
}

object CirceRefinedProtocolGenerator {
  def apply(circeRefinedVersion: CirceRefinedModelGenerator): ProtocolTerms[ScalaLanguage, Target] =
    fromGenerator(CirceProtocolGenerator.withValidations(circeRefinedVersion.toCirce, applyValidations))

  def applyValidations(className: String, tpe: Type, prop: Tracker[Schema[_]]): Target[Type] = {
    import scala.meta._
    tpe match {
      case t"String" =>
        prop.downField("pattern", _.getPattern).fold(Target.pure(tpe)) { patternTracker =>
          Try {
            val pat     = patternTracker.unwrapTracker
            val refined = s"""_root_.eu.timepit.refined.string.MatchesRegex[$className.`"$pat"`.T]""".parse[Type].get
            t"""String Refined $refined"""
          }.fold(th => Target.raiseUserError(s"$th: ${patternTracker.showHistory}"), Target.pure)
        }
      case t"Int" =>
        def refine(decimal: BigDecimal): Type = Type.Select(Term.Select(q"_root_.shapeless.Witness", Term.Name(decimal.toInt.toString)), t"T")
        val maxOpt                            = prop.downField("maximum", _.getMaximum).unwrapTracker.map(refine(_)) // Can't use ETA since we need ...
        val minOpt                            = prop.downField("mimimum", _.getMinimum).unwrapTracker.map(refine(_)) // Scala's BigDecimal, not java.math
        val rawType = (maxOpt, minOpt) match {
          case (Some(max), Some(min)) =>
            val refined = t"_root_.eu.timepit.refined.numeric.Interval.Closed[$min, $max]"
            t"""$tpe Refined $refined"""
          case (Some(max), None) =>
            val refined = t"_root_.eu.timepit.refined.numeric.LessEqual[$max]"
            t"""$tpe Refined $refined"""
          case (None, Some(min)) =>
            val refined = t"_root_.eu.timepit.refined.numeric.GreaterEqual[$min]"
            t"""$tpe Refined $refined"""
          case _ => tpe
        }
        Target.pure(rawType)
      case _ => Target.pure(tpe)
    }
  }

  def fromGenerator(generator: ProtocolTerms[ScalaLanguage, Target]): ProtocolTerms[ScalaLanguage, Target] =
    generator.copy(
      protocolImports = { () =>
        generator
          .protocolImports()
          .map(imports =>
            imports ++ List(
              q"import io.circe.refined._",
              q"import eu.timepit.refined.api.Refined",
              q"import eu.timepit.refined.auto._"
            )
          )
      },
      staticProtocolImports = pkgName => {
        val implicitsRef: Term.Ref = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
        Target.pure(
          List(
            q"import cats.implicits._",
            q"import cats.data.EitherT",
            q"import io.circe.refined._",
            q"import eu.timepit.refined.api.Refined",
            q"import eu.timepit.refined.auto._"
          ) :+ q"import $implicitsRef._"
        )
      }
    )

}
