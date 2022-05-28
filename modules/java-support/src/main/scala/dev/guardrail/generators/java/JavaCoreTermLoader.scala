package dev.guardrail.generators.java

import cats.data.NonEmptyList
import dev.guardrail.{ Args, Target }
import dev.guardrail.generators.spi.CoreTermsLoader
import scala.reflect.runtime.universe.typeTag

class JavaCoreTermLoader extends CoreTermsLoader {
  type L = JavaLanguage
  def reified = typeTag[Target[JavaLanguage]]
  def apply(language: String, parameters: NonEmptyList[Args]) =
    if (language == "java") Some(() => runM(parameters)(JavaGeneratorMappings.javaInterpreter)) else None
}
