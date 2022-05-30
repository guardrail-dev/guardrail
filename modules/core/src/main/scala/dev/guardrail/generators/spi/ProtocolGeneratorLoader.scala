package dev.guardrail.generators.spi

import dev.guardrail.terms.ProtocolTerms
import java.util.ServiceLoader

trait ProtocolGeneratorLoader extends AbstractGeneratorLoader[ProtocolTerms]

object ProtocolGeneratorLoader extends AbstractGeneratorLoaderCompanion[ProtocolTerms, ProtocolGeneratorLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def protocolLoader: ServiceLoader[ProtocolGeneratorLoader] = loader
}
