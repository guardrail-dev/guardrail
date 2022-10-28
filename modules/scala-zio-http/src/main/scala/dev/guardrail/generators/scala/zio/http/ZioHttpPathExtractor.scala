package dev.guardrail.generators.scala.zio.http

import scala.meta._

import dev.guardrail.core.PathExtractor
import dev.guardrail.generators.scala.{ ModelGeneratorType, ScalaLanguage }
import dev.guardrail.generators.LanguageParameter

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object ZioHttpPathExtractor
    extends PathExtractor[ScalaLanguage, Pat, Term.Name, ModelGeneratorType](
      pathSegmentConverter = { case (LanguageParameter(_, param, paramName, argName, argType), base, _) =>
        base.fold[Either[String, Pat]] {
          argType match {
            case t"Int"                         => Right(p"IntVar(${Pat.Var(paramName)})")
            case t"Long"                        => Right(p"LongVar(${Pat.Var(paramName)})")
            case t"String"                      => Right(Pat.Var(paramName))
            case t"java.util.UUID"              => Right(p"UUIDVar(${Pat.Var(paramName)})")
            case Type.Name(tpe)                 => Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
            case Type.Select(_, Type.Name(tpe)) => Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
            case tpe =>
              println(s"Doing our best turning ${tpe} into an extractor")
              Right(p"${Term.Name(s"${tpe}Var")}(${Pat.Var(paramName)})")
          }
        } { _ =>
          // todo add support for regex segment
          Left("Unsupported feature")
        }
      },
      buildParamConstraint = { case (k, v) =>
        p"${Term.Name(s"${k.capitalize}Matcher")}(${Lit.String(v)})"
      },
      joinParams = { (l, r) =>
        p"${l} +& ${r}"
      },
      stringPath = Lit.String(_),
      liftBinding = identity,
      litRegex = (before, _, after) =>
        // todo add support for regex segment
        throw new UnsupportedOperationException
    )
