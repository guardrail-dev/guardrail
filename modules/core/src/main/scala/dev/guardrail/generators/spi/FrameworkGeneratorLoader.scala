package dev.guardrail.generators.spi

import dev.guardrail.terms.framework.FrameworkTerms
import java.util.ServiceLoader

trait FrameworkGeneratorLoader extends AbstractGeneratorLoader[FrameworkTerms]

object FrameworkGeneratorLoader extends AbstractGeneratorLoaderCompanion[FrameworkTerms, FrameworkGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def frameworkLoader: ServiceLoader[FrameworkGeneratorLoader] = loader
}
