package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.{ JacksonModelGenerator, ScalaCollectionsGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.jackson.JacksonProtocolGenerator
import dev.guardrail.generators.scala.{ AkkaHttpVersion, ScalaGenerator }
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttpJackson extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
  implicit def ProtocolInterp       = JacksonProtocolGenerator.apply
  implicit def ClientInterp         = AkkaHttpClientGenerator(JacksonModelGenerator)
  implicit def FrameworkInterp      = AkkaHttpGenerator(AkkaHttpVersion.V10_2, JacksonModelGenerator)
  implicit def ServerInterp         = AkkaHttpServerGenerator(AkkaHttpVersion.V10_2, JacksonModelGenerator)
  implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]()
  implicit def LanguageInterp       = ScalaGenerator()
}
