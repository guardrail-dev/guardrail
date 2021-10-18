package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.{ JacksonModelGenerator, ScalaCollectionsGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.jackson.JacksonProtocolGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttpJackson extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
  implicit def ProtocolInterp       = JacksonProtocolGenerator.apply
  implicit def ClientInterp         = AkkaHttpClientGenerator(JacksonModelGenerator)
  implicit def FrameworkInterp      = AkkaHttpGenerator(JacksonModelGenerator)
  implicit def ServerInterp         = AkkaHttpServerGenerator(JacksonModelGenerator)
  implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp       = ScalaGenerator()
}
