package dev.guardrail.generators.spi

import dev.guardrail.terms.CollectionsLibTerms
import java.util.ServiceLoader

trait CollectionsGeneratorLoader extends AbstractGeneratorLoader[CollectionsLibTerms, Set[String]]

object CollectionsGeneratorLoader extends AbstractGeneratorLoaderCompanion[CollectionsLibTerms, Set[String], CollectionsGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def collectionsLoader: ServiceLoader[CollectionsGeneratorLoader] = loader
}
