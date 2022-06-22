package dev.guardrail.generators.spi

import dev.guardrail.terms.server.ServerTerms

trait ServerGeneratorLoader extends AbstractGeneratorLoader[ServerTerms]

object ServerGeneratorLoader extends AbstractGeneratorLoaderCompanion[ServerTerms, ServerGeneratorLoader] {
  val label: String = "ServerGenerator"
}
