package dev.guardrail.generators.spi

import dev.guardrail.terms.server.ServerTerms
import java.util.ServiceLoader

trait ServerGeneratorLoader extends AbstractGeneratorLoader[ServerTerms, Set[String]]

object ServerGeneratorLoader extends AbstractGeneratorLoaderCompanion[ServerTerms, Set[String], ServerGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def serverLoader: ServiceLoader[ServerGeneratorLoader] = loader
}
