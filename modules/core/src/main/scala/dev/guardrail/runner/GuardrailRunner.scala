package dev.guardrail.runner

import java.nio.file.Path
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.io.AnsiColor

import dev.guardrail.{ Args, MissingDependency, ReadSpec, Target, WriteTree }
import dev.guardrail.core.StructuredLogger
import dev.guardrail.generators.spi.CoreTermsLoader

abstract class GuardrailRunner {
  def guardrailRunner(tasks: Map[String, NonEmptyList[Args]]): Target[List[java.nio.file.Path]] =
    tasks.toList
      .flatTraverse { case (language, args) =>
        CoreTermsLoader
          .load(language, args, MissingDependency(s"${language}-support"))
          .map(_.toList)
      }
      .flatMap(
        _.flatTraverse(rs =>
          ReadSpec
            .readSpec(rs)
            .flatMap(_.traverse(WriteTree.writeTree))
            .leftFlatMap(value =>
              Target.pushLogger(StructuredLogger.error(s"${AnsiColor.RED}Error in ${rs.path}${AnsiColor.RESET}")) *> Target.raiseError[List[Path]](value)
            )
            .productL(Target.pushLogger(StructuredLogger.reset))
        )
      )
      .map(_.distinct)
}
