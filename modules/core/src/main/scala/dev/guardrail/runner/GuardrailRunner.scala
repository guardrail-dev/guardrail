package dev.guardrail.runner

import java.nio.file.Path
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.io.AnsiColor

import dev.guardrail.{ Args, MissingDependency, NoArgsSpecified, ReadSpec, Target, WriteTree }
import dev.guardrail.core.StructuredLogger
import dev.guardrail.generators.spi.CoreTermsLoader

abstract class GuardrailRunner {
  def guardrailRunner(language: String, args: Array[Args]): Target[List[Path]] =
    for {
      args            <- Target.fromOption(NonEmptyList.fromList(args.toList), NoArgsSpecified)
      coreTermsLoader <- CoreTermsLoader.load(language, args, MissingDependency(s"${language}-support"))
      result <- coreTermsLoader.toList.flatTraverse(rs =>
        ReadSpec
          .readSpec(rs)
          .flatMap(_.traverse(WriteTree.writeTree))
          .leftFlatMap(value =>
            Target.pushLogger(StructuredLogger.error(s"${AnsiColor.RED}Error in ${rs.path}${AnsiColor.RESET}")) *> Target.raiseError[List[Path]](value)
          )
          .productL(Target.pushLogger(StructuredLogger.reset))
      )
    } yield result

  def unsafeGuardrailRunner(language: String, args: Array[Args]): Array[Path] =
    Target.unsafeExtract(guardrailRunner(language, args)).toArray
}
