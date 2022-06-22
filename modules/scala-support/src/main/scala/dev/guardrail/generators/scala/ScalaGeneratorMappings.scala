package dev.guardrail.generators.scala

import cats.syntax.all._
import _root_.scala.meta._

import dev.guardrail.core.CoreTermInterp
import dev.guardrail.generators.spi.{ FrameworkLoader, ModuleMapperLoader }
import dev.guardrail.UnparseableArgument

object ScalaGeneratorMappings {
  implicit def scalaInterpreter = new CoreTermInterp[ScalaLanguage](
    "akka-http",
    FrameworkLoader.load[ScalaLanguage](_),
    frameworkName => ModuleMapperLoader.load[ScalaLanguage](frameworkName),
    _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
  )
}
