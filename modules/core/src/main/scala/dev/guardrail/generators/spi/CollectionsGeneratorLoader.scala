package dev.guardrail.generators.spi

import dev.guardrail.terms.CollectionsLibTerms

trait CollectionsGeneratorLoader extends AbstractGeneratorLoader[CollectionsLibTerms]

object CollectionsGeneratorLoader extends AbstractGeneratorLoaderCompanion[CollectionsLibTerms, CollectionsGeneratorLoader] {
  val label: String = "CollectionsGenerator"
}
