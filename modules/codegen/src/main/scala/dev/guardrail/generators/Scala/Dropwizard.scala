package dev.guardrail.generators.Scala

import dev.guardrail.Target
import dev.guardrail.generators.ScalaGenerator.ScalaInterp
import dev.guardrail.generators.collections.ScalaCollectionsGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.client.ClientTerms
import dev.guardrail.protocol.terms.protocol._
import dev.guardrail.protocol.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }

object Dropwizard extends Framework[ScalaLanguage, Target] {
  override implicit def ArrayProtocolInterp: ArrayProtocolTerms[ScalaLanguage, Target]     = JacksonProtocolGenerator.ArrayProtocolTermInterp
  override implicit def ClientInterp: ClientTerms[ScalaLanguage, Target]                   = DropwizardClientGenerator.ClientTermInterp
  override implicit def EnumProtocolInterp: EnumProtocolTerms[ScalaLanguage, Target]       = JacksonProtocolGenerator.EnumProtocolTermInterp
  override implicit def FrameworkInterp: FrameworkTerms[ScalaLanguage, Target]             = DropwizardGenerator.FrameworkInterp
  override implicit def ModelProtocolInterp: ModelProtocolTerms[ScalaLanguage, Target]     = JacksonProtocolGenerator.ModelProtocolTermInterp
  override implicit def PolyProtocolInterp: PolyProtocolTerms[ScalaLanguage, Target]       = JacksonProtocolGenerator.PolyProtocolTermInterp
  override implicit def ProtocolSupportInterp: ProtocolSupportTerms[ScalaLanguage, Target] = JacksonProtocolGenerator.ProtocolSupportTermInterp
  override implicit def ServerInterp: ServerTerms[ScalaLanguage, Target]                   = DropwizardServerGenerator.ServerTermInterp
  override implicit def SwaggerInterp: SwaggerTerms[ScalaLanguage, Target]                 = SwaggerGenerator[ScalaLanguage]()
  override implicit def LanguageInterp: LanguageTerms[ScalaLanguage, Target]               = ScalaInterp
  override implicit def CollectionsLibInterp: CollectionsLibTerms[ScalaLanguage, Target]   = ScalaCollectionsGenerator.ScalaCollectionsInterp
}
