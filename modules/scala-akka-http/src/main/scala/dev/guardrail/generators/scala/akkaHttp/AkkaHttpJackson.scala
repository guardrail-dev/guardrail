package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.{ JacksonModelGenerator, ScalaCollectionsGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.jackson.JacksonProtocolGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttpJackson extends Framework[ScalaLanguage, Target] {
  override implicit def ProtocolInterp       = JacksonProtocolGenerator.apply
  override implicit def ClientInterp         = AkkaHttpClientGenerator(JacksonModelGenerator)
  override implicit def FrameworkInterp      = AkkaHttpGenerator(AkkaHttpVersion.V10_2, JacksonModelGenerator)
  override implicit def ServerInterp         = AkkaHttpServerGenerator(AkkaHttpVersion.V10_2, JacksonModelGenerator)
  override implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]()
  override implicit def LanguageInterp       = ScalaGenerator()
  override implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
}
