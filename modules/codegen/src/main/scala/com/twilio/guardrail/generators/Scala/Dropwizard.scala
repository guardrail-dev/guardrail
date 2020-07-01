package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.Scala.DropwizardClientGenerator.ClientTermInterp
import com.twilio.guardrail.generators.Scala.DropwizardServerGenerator.ServerTermInterp
import com.twilio.guardrail.generators.Scala.JacksonProtocolGenerator._
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
  override implicit def ArrayProtocolInterp: ArrayProtocolTerms[ScalaLanguage, Target]     = ArrayProtocolTermInterp
  override implicit def ClientInterp: ClientTerms[ScalaLanguage, Target]                   = ClientTermInterp
  override implicit def EnumProtocolInterp: EnumProtocolTerms[ScalaLanguage, Target]       = EnumProtocolTermInterp
  override implicit def FrameworkInterp: FrameworkTerms[ScalaLanguage, Target]             = DropwizardGenerator.FrameworkInterp
  override implicit def ModelProtocolInterp: ModelProtocolTerms[ScalaLanguage, Target]     = ModelProtocolTermInterp
  override implicit def PolyProtocolInterp: PolyProtocolTerms[ScalaLanguage, Target]       = PolyProtocolTermInterp
  override implicit def ProtocolSupportInterp: ProtocolSupportTerms[ScalaLanguage, Target] = ProtocolSupportTermInterp
  override implicit def ServerInterp: ServerTerms[ScalaLanguage, Target]                   = ServerTermInterp
  override implicit def SwaggerInterp: SwaggerTerms[ScalaLanguage, Target]                 = SwaggerGenerator[ScalaLanguage]()
  override implicit def LanguageInterp: LanguageTerms[ScalaLanguage, Target]               = ScalaInterp
  override implicit def CollectionsLibInterp: CollectionsLibTerms[ScalaLanguage, Target]   = ScalaCollectionsGenerator.ScalaCollectionsInterp
}
