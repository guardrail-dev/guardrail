package dev.guardrail.generators.spi

import dev.guardrail.terms.CollectionsLibTerms
import java.util.ServiceLoader

trait CollectionsGeneratorLoader extends AbstractGeneratorLoader[CollectionsLibTerms]

object CollectionsGeneratorLoader extends AbstractGeneratorLoaderCompanion[CollectionsLibTerms, CollectionsGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def collectionsLoader: ServiceLoader[CollectionsGeneratorLoader] = loader
}
