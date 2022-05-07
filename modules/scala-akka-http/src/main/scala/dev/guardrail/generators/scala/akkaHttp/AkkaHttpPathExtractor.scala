package dev.guardrail.generators.scala.akkaHttp

import scala.meta._

import dev.guardrail.core.PathExtractor
import dev.guardrail.generators.scala.{ ModelGeneratorType, ScalaLanguage }
import dev.guardrail.generators.LanguageParameter

object AkkaHttpPathExtractor
    extends PathExtractor[ScalaLanguage, Term, Term.Name, ModelGeneratorType](
      pathSegmentConverter = { case (LanguageParameter(_, param, _, argName, argType), base, modelGeneratorType) =>
        base.fold {
          argType match {
            case t"String" => Right(q"Segment")
            case t"Double" => Right(q"DoubleNumber")
            case t"BigDecimal" =>
              Right(q"Segment.map(BigDecimal.apply _)")
            case t"Int"    => Right(q"IntNumber")
            case t"Long"   => Right(q"LongNumber")
            case t"BigInt" => Right(q"Segment.map(BigInt.apply _)")
            case tpe =>
              Right(q"Segment.flatMap(str => ${AkkaHttpHelper.fromStringConverter(tpe, modelGeneratorType)})")
          }
        } { segment =>
          argType match {
            case t"String" => Right(segment)
            case t"BigDecimal" =>
              Right(q"${segment}.map(BigDecimal.apply _)")
            case t"BigInt" => Right(q"${segment}.map(BigInt.apply _)")
            case tpe =>
              Right(q"${segment}.flatMap(str => ${AkkaHttpHelper.fromStringConverter(tpe, modelGeneratorType)})")
          }
        }
      },
      buildParamConstraint = { case (k, v) =>
        q" parameter(${Lit.String(k)}).require(_ == ${Lit.String(v)}) "
      },
      joinParams = { (l, r) =>
        q"${l} & ${r}"
      },
      stringPath = Lit.String(_),
      liftBinding = identity,
      litRegex = (before, _, after) =>
        q"""new scala.util.matching.Regex("^" + ${Lit
            .String(before)} + "(.*)" + ${Lit.String(after)} + ${Lit.String("$")})"""
    )
