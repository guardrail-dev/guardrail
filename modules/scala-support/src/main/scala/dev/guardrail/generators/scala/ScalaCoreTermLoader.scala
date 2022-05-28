package dev.guardrail.generators.scala

import cats.data.NonEmptyList
import dev.guardrail.{ Args, Target }
import dev.guardrail.generators.spi.CoreTermsLoader
import scala.reflect.runtime.universe.typeTag

class ScalaCoreTermLoader extends CoreTermsLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  def apply(language: String, parameters: NonEmptyList[Args]) =
    if (language == "scala") Some(() => runM(parameters)(ScalaGeneratorMappings.scalaInterpreter)) else None
}
