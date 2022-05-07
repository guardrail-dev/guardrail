package dev.guardrail.runner

import java.nio.file.Path
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.io.AnsiColor

import dev.guardrail.{ Args, ReadSwagger, Target, UnparseableArgument, WriteTree }
import dev.guardrail.core.StructuredLogger

trait GuardrailRunner {
  def languages: Map[String, NonEmptyList[Args] => Target[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]]]

  def guardrailRunner: Map[String, NonEmptyList[Args]] => Target[List[java.nio.file.Path]] = { tasks =>
    runLanguages(tasks)
      .flatMap(
        _.flatTraverse(rs =>
          ReadSwagger
            .readSwagger(rs)
            .flatMap(_.traverse(WriteTree.writeTree))
            .leftFlatMap(value =>
              Target.pushLogger(StructuredLogger.error(s"${AnsiColor.RED}Error in ${rs.path}${AnsiColor.RESET}")) *> Target.raiseError[List[Path]](value)
            )
            .productL(Target.pushLogger(StructuredLogger.reset))
        )
      )
      .map(_.distinct)
  }

  def runLanguages(tasks: Map[String, NonEmptyList[Args]]): Target[List[ReadSwagger[Target[List[WriteTree]]]]] =
    tasks.toList.flatTraverse { case (language, args) =>
      languages.get(language) match {
        case None       => Target.raiseError(UnparseableArgument("language", language))
        case Some(func) => func(args).map(_.toList)
      }
    }
}
