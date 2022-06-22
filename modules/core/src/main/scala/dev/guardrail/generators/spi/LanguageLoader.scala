package dev.guardrail.generators.spi

import dev.guardrail.terms.LanguageTerms

trait LanguageLoader extends AbstractGeneratorLoader[LanguageTerms]

object LanguageLoader extends AbstractGeneratorLoaderCompanion[LanguageTerms, LanguageLoader] {
  val label: String = "Language"
}
