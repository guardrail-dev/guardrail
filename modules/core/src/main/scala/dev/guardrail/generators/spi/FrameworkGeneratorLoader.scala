package dev.guardrail.generators.spi

import dev.guardrail.terms.framework.FrameworkTerms

trait FrameworkGeneratorLoader extends AbstractGeneratorLoader[FrameworkTerms]

object FrameworkGeneratorLoader extends AbstractGeneratorLoaderCompanion[FrameworkTerms, FrameworkGeneratorLoader] {
  val label: String = "FrameworkGenerator"
}
