package dev.guardrail.generators.scala.zio.http

import dev.guardrail.core.PathExtractor
import dev.guardrail.generators.LanguageParameter
import dev.guardrail.generators.scala.ModelGeneratorType
import dev.guardrail.generators.scala.ScalaLanguage

import scala.meta._

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object ZioHttpPathExtractor
    extends PathExtractor[ScalaLanguage, Term, Term.Name, ModelGeneratorType](
      pathSegmentConverter = { case (LanguageParameter(_, param, paramName, argName, argType), base, _) =>
        base.fold[Either[String, Term]] {
          argType match {
            case t"Int"                         => Right(q"int(${Lit.String(paramName.value)})")
            case t"Long"                        => Right(q"long(${Lit.String(paramName.value)})")
            case t"Boolean"                     => Right(q"boolean(${Lit.String(paramName.value)})")
            case t"String"                      => Right(q"string(${Lit.String(paramName.value)})")
            case t"java.util.UUID"              => Right(q"uuid(${Lit.String(paramName.value)})")
            case Type.Name(tpe)                 => Right(q"${Term.Name(s"${tpe}")}(${Lit.String(paramName.value)})")
            case Type.Select(_, Type.Name(tpe)) => Right(q"${Term.Name(s"${tpe}")}(${Lit.String(paramName.value)})")
            case tpe =>
              println(s"Doing our best turning ${tpe} into an extractor")
              Right(q"${Term.Name(s"${tpe}")}(${Lit.String(paramName.value)})")
          }
        } { _ =>
          // todo add support for regex segment
          Left("Unsupported feature")
        }
      },
      buildParamConstraint = { case (k, v) =>
        q"${Term.Name(s"${k.capitalize}Matcher")}(${Lit.String(v)})"
      },
      joinParams = { (l, r) =>
        q"${l} +& ${r}"
      },
      stringPath = Lit.String(_),
      liftBinding = identity,
      litRegex = (before, _, after) =>
        // todo add support for regex segment
        throw new UnsupportedOperationException
    )
