package dev.guardrail.generators.scala

import cats.data.NonEmptyList
import scala.meta._

import dev.guardrail.Target
import dev.guardrail.core.{ PathExtractor, ReifiedRawType, Tracker }
import dev.guardrail.generators.operations.TracingLabelFormatter
import dev.guardrail.generators.syntax.GENERATED_CODE_COMMENT_LINES
import dev.guardrail.generators.{ LanguageParameter, RawParameterName }
import dev.guardrail.terms.protocol.StaticDefns

package object syntax {
  implicit class RichRawParameterName(parameter: RawParameterName) {
    import _root_.scala.meta._
    def toLit: Lit.String = Lit.String(parameter.value)
  }

  implicit class ScalaTracingLabel(value: TracingLabelFormatter) {
    def toLit: Lit.String = Lit.String(s"${value.context}:${value.operationId}")
  }

  implicit class RichLanguageParameter(value: LanguageParameter.type) {
    import _root_.scala.meta._
    @SuppressWarnings(Array("org.wartremover.warts.TripleQuestionMark"))
    def fromParam(param: Term.Param, rawType: Option[String] = Some("string"), rawFormat: Option[String] = None): LanguageParameter[ScalaLanguage] =
      param match {
        case param @ Term.Param(_, name, decltype, _) =>
          val tpe: Type = decltype
            .flatMap {
              case tpe @ t"Option[$_]" => Some(tpe)
              case Type.ByName(tpe)    => Some(tpe)
              case tpe @ Type.Name(_)  => Some(tpe)
              case _                   => None
            }
            .getOrElse(t"Nothing")
          new LanguageParameter[ScalaLanguage](
            None,
            param,
            Term.Name(name.value),
            RawParameterName(name.value),
            tpe,
            ReifiedRawType.of(rawType, rawFormat),
            true,
            None,
            false
          )
        case _: Term.Param => ??? // Impossible, see https://github.com/scalameta/scalameta/pull/2501
      }
  }

  implicit class ExtendedUnzip[T1, T2, T3, T4, T5, T6, T7, T8](xs: NonEmptyList[(T1, T2, T3, T4, T5, T6, T7, T8)]) {
    def unzip8: (List[T1], List[T2], List[T3], List[T4], List[T5], List[T6], List[T7], List[T8]) =
      xs.foldLeft(
        (List.empty[T1], List.empty[T2], List.empty[T3], List.empty[T4], List.empty[T5], List.empty[T6], List.empty[T7], List.empty[T8])
      ) { case ((v1a, v2a, v3a, v4a, v5a, v6a, v7a, v8a), (v1, v2, v3, v4, v5, v6, v7, v8)) =>
        (v1a :+ v1, v2a :+ v2, v3a :+ v3, v4a :+ v4, v5a :+ v5, v6a :+ v6, v7a :+ v7, v8a :+ v8)
      }
  }

  val GENERATED_CODE_COMMENT: String =
    s"""/*
       |${GENERATED_CODE_COMMENT_LINES.mkString(" * ", "\n| * ", "")}
       | */
       |""".stripMargin

  def companionForStaticDefns(staticDefns: StaticDefns[ScalaLanguage]): Defn.Object =
    q"""
    object ${Term.Name(staticDefns.className)} {
      ..${staticDefns.extraImports}
      ..${staticDefns.definitions}
      ..${staticDefns.statements}
    }
    """

  def generateUrlPathParams(path: Tracker[String], pathArgs: List[LanguageParameter[ScalaLanguage]]): Target[Term] =
    PathExtractor.generateUrlPathParams[ScalaLanguage](
      path,
      pathArgs,
      Lit.String(_),
      name => q"Formatter.addPath(${name})",
      q"host + basePath",
      (a, b) => q"${a} + ${b}"
    )
}
