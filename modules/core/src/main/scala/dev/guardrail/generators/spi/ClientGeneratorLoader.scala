package dev.guardrail.generators.spi

import dev.guardrail.terms.client.ClientTerms
import java.util.ServiceLoader

trait ClientGeneratorLoader extends AbstractGeneratorLoader[ClientTerms, Set[String]]

object ClientGeneratorLoader extends AbstractGeneratorLoaderCompanion[ClientTerms, Set[String], ClientGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def clientLoader: ServiceLoader[ClientGeneratorLoader] = loader
}
