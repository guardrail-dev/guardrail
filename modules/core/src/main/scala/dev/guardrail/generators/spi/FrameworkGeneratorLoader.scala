package dev.guardrail.generators.spi

import dev.guardrail.terms.framework.FrameworkTerms
import java.util.ServiceLoader

trait FrameworkGeneratorLoader extends AbstractGeneratorLoader[FrameworkTerms, Set[String]]

object FrameworkGeneratorLoader extends AbstractGeneratorLoaderCompanion[FrameworkTerms, Set[String], FrameworkGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def frameworkLoader: ServiceLoader[FrameworkGeneratorLoader] = loader
}
