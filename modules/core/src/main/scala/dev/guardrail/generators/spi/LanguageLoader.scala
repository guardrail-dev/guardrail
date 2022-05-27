package dev.guardrail.generators.spi

import dev.guardrail.terms.LanguageTerms
import java.util.ServiceLoader

trait LanguageLoader extends AbstractGeneratorLoader[LanguageTerms, Set[String]]

object LanguageLoader extends AbstractGeneratorLoaderCompanion[LanguageTerms, Set[String], LanguageLoader] {
  @deprecated("Deprecated in favor of an abstract 'loader' member", "0.71.2")
  def frameworkLoader: ServiceLoader[LanguageLoader] = loader
}
