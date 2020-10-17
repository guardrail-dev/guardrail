package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.ScalaGenerator.ScalaInterp
import com.twilio.guardrail.generators.collections.ScalaCollectionsGenerator
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }

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
