package dev.guardrail.generators.java

import scala.reflect.runtime.universe.typeTag

import dev.guardrail.Target
import dev.guardrail.generators.spi.FrameworkLoader

class JavaFrameworkLoader extends FrameworkLoader {
  type L = JavaLanguage
  def reified = typeTag[Target[JavaLanguage]]
}
