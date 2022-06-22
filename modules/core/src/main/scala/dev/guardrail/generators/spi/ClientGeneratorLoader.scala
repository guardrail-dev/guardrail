package dev.guardrail.generators.spi

import dev.guardrail.terms.client.ClientTerms

trait ClientGeneratorLoader extends AbstractGeneratorLoader[ClientTerms]

object ClientGeneratorLoader extends AbstractGeneratorLoaderCompanion[ClientTerms, ClientGeneratorLoader] {
  val label: String = "ClientGenerator"
}
