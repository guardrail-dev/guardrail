package dev.guardrail.generators.scala.circe

import _root_.io.swagger.v3.oas.models.media.Schema
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.generators.scala.{ CirceRefinedModelGenerator, ScalaLanguage }
import dev.guardrail.generators.spi.{ ModuleLoadResult, ProtocolGeneratorLoader }
import dev.guardrail.terms.ProtocolTerms
import dev.guardrail.terms.protocol._
import cats.implicits._

import scala.meta._
import scala.reflect.runtime.universe.typeTag

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
      case raw @ t"Vector[$inner]" =>
        def refine(decimal: Integer): Type = Type.Select(Term.Select(q"_root_.shapeless.Witness", Term.Name(decimal.toInt.toString)), t"T")
        val maxOpt                         = prop.downField("maxItems", _.getMaxItems).unwrapTracker.map(refine)
        val minOpt                         = prop.downField("minItems", _.getMinItems).unwrapTracker.map(refine)

        val intervalOpt = (maxOpt, minOpt) match {
          case (Some(max), Some(min)) =>
            Some(t"_root_.eu.timepit.refined.numeric.Interval.Closed[$min, $max]")
          case (Some(max), None) =>
            Some(t"_root_.eu.timepit.refined.numeric.LessEqual[$max]")
          case (None, Some(min)) =>
            Some(t"_root_.eu.timepit.refined.numeric.GreaterEqual[$min]")
          case _ => None
        }

        for {
          validatedVectorType <- prop.downField("items", _.getItems).indexedDistribute.traverse { tracker =>
            applyValidations(className, inner, tracker)
              .map(vectorElementType => t"Vector[$vectorElementType]")
          }
        } yield {
          val vectorType = validatedVectorType.getOrElse(raw)
          intervalOpt.fold[Type](vectorType) { interval =>
            t"""$vectorType Refined _root_.eu.timepit.refined.collection.Size[$interval]"""
          }
        }

      case t"String" =>
        prop
          .downField("pattern", _.getPattern)
          .indexedDistribute
          .filter(_.unwrapTracker.nonEmpty)
          .fold(tpe) { patternTracker =>
            val pat     = patternTracker.unwrapTracker
            val prepend = if (pat.startsWith("^")) "" else ".*"
            val append  = if (pat.endsWith("$")) "" else ".*"

            val refined =
              Type.Apply(
                t"_root_.eu.timepit.refined.string.MatchesRegex",
                List(Type.Select(Term.Select(Term.Name(className), Term.Name(s""""$prepend$pat$append"""")), t"T"))
              )
            t"""String Refined $refined"""
          }
          .pure[Target]
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

  def renderDTOStaticDefns(
      base: ProtocolTerms[ScalaLanguage, Target]
  )(
      clsName: String,
      deps: List[scala.meta.Term.Name],
      encoder: Option[scala.meta.Defn.Val],
      decoder: Option[scala.meta.Defn.Val],
      protocolParameters: List[ProtocolParameter[ScalaLanguage]]
  ) = {
    val regexHelperTypes: List[Defn.Val] =
      protocolParameters
        .flatMap(_.propertyValidation.map(_.regex).indexedDistribute)
        .filter(_.unwrapTracker.nonEmpty)
        .map { patternTracker: Tracker[String] =>
          val pattern                 = patternTracker.unwrapTracker
          val prepend                 = if (pattern.startsWith("^")) "" else ".*"
          val append                  = if (pattern.endsWith("$")) "" else ".*"
          val partiallyMatchedPattern = s"$prepend$pattern$append"
          val name                    = Term.Name(s""""$partiallyMatchedPattern"""")

          q"val ${Pat.Var(name)} = _root_.shapeless.Witness(${Lit.String(partiallyMatchedPattern)})"
        }
    for {
      defns <- base.renderDTOStaticDefns(clsName, deps, encoder, decoder, protocolParameters)
    } yield defns.copy(definitions = regexHelperTypes ++ defns.definitions)
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
      renderDTOStaticDefns = renderDTOStaticDefns(generator) _,
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
