package dev.guardrail.generators.scala

import scala.reflect.runtime.universe.typeTag

import dev.guardrail.Target
import dev.guardrail.generators.spi.FrameworkLoader

class ScalaFrameworkLoader extends FrameworkLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
}
