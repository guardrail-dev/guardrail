package dev.guardrail.generators

import cats.data.NonEmptyList

import dev.guardrail.generators.java.JavaGeneratorMappings.javaInterpreter
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.{ Args, Common, ReadSwagger, Target, WriteTree }

object GeneratorMappings {
  def defaultLanguages: Map[String, NonEmptyList[Args] => Target[NonEmptyList[ReadSwagger[Target[List[WriteTree]]]]]] = Map(
    ("java", Common.runM[JavaLanguage, Target](_)),
    ("scala", Common.runM[ScalaLanguage, Target](_))
  )
}
