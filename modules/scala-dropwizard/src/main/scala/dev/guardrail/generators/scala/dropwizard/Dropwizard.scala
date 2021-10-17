package dev.guardrail.generators.scala.dropwizard

import dev.guardrail.Target
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.jackson._
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, ProtocolTerms, SwaggerTerms }

object Dropwizard extends Framework[ScalaLanguage, Target] {
  override implicit def ClientInterp: ClientTerms[ScalaLanguage, Target]                 = new DropwizardClientGenerator
  override implicit def FrameworkInterp: FrameworkTerms[ScalaLanguage, Target]           = new DropwizardGenerator
  override implicit def ProtocolInterp: ProtocolTerms[ScalaLanguage, Target]             = JacksonProtocolGenerator.apply
  override implicit def ServerInterp: ServerTerms[ScalaLanguage, Target]                 = new DropwizardServerGenerator
  override implicit def SwaggerInterp: SwaggerTerms[ScalaLanguage, Target]               = SwaggerGenerator[ScalaLanguage]()
  override implicit def LanguageInterp: LanguageTerms[ScalaLanguage, Target]             = ScalaGenerator
  override implicit def CollectionsLibInterp: CollectionsLibTerms[ScalaLanguage, Target] = ScalaCollectionsGenerator
}
