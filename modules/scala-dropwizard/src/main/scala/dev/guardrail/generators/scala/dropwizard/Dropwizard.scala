package dev.guardrail.generators.scala.dropwizard

import dev.guardrail.Target
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.jackson._
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }

object Dropwizard extends Framework[ScalaLanguage, Target] {
  override implicit def ArrayProtocolInterp: ArrayProtocolTerms[ScalaLanguage, Target]     = JacksonProtocolGenerator.ArrayProtocolTermInterp
  override implicit def ClientInterp: ClientTerms[ScalaLanguage, Target]                   = new DropwizardClientGenerator
  override implicit def EnumProtocolInterp: EnumProtocolTerms[ScalaLanguage, Target]       = JacksonProtocolGenerator.EnumProtocolTermInterp
  override implicit def FrameworkInterp: FrameworkTerms[ScalaLanguage, Target]             = new DropwizardGenerator
  override implicit def ModelProtocolInterp: ModelProtocolTerms[ScalaLanguage, Target]     = JacksonProtocolGenerator.ModelProtocolTermInterp
  override implicit def PolyProtocolInterp: PolyProtocolTerms[ScalaLanguage, Target]       = JacksonProtocolGenerator.PolyProtocolTermInterp
  override implicit def ProtocolSupportInterp: ProtocolSupportTerms[ScalaLanguage, Target] = JacksonProtocolGenerator.ProtocolSupportTermInterp
  override implicit def ServerInterp: ServerTerms[ScalaLanguage, Target]                   = new DropwizardServerGenerator
  override implicit def SwaggerInterp: SwaggerTerms[ScalaLanguage, Target]                 = SwaggerGenerator[ScalaLanguage]()
  override implicit def LanguageInterp: LanguageTerms[ScalaLanguage, Target]               = ScalaGenerator
  override implicit def CollectionsLibInterp: CollectionsLibTerms[ScalaLanguage, Target]   = ScalaCollectionsGenerator
}
