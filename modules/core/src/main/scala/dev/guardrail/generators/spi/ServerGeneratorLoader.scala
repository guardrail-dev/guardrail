package dev.guardrail.generators.spi

import dev.guardrail.terms.server.ServerTerms
import java.util.ServiceLoader

trait ServerGeneratorLoader extends AbstractGeneratorLoader[ServerTerms]

object ServerGeneratorLoader extends AbstractGeneratorLoaderCompanion[ServerTerms, ServerGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def serverLoader: ServiceLoader[ServerGeneratorLoader] = loader
}
