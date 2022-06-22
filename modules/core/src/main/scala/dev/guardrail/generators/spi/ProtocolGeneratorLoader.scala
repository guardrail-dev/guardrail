package dev.guardrail.generators.spi

import dev.guardrail.terms.ProtocolTerms

trait ProtocolGeneratorLoader extends AbstractGeneratorLoader[ProtocolTerms]

object ProtocolGeneratorLoader extends AbstractGeneratorLoaderCompanion[ProtocolTerms, ProtocolGeneratorLoader] {
  val label: String = "ProtocolGenerator"
}
