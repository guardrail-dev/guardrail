package dev.guardrail.runner

import java.nio.file.Path
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.io.AnsiColor

import dev.guardrail.{ Args, MissingDependency, ReadSwagger, Target, WriteTree }
import dev.guardrail.core.StructuredLogger
import dev.guardrail.generators.spi.CoreTermsLoader

trait GuardrailRunner {
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
      CoreTermsLoader
        .load(language, args, MissingDependency(s"${language}-support"))
        .map(_.toList)
    }
}
