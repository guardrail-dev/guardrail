package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.{ JacksonModelGenerator, ScalaCollectionsGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.jackson.JacksonProtocolGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttpJackson extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsGenerator
  implicit def ArrayProtocolInterp   = JacksonProtocolGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = new AkkaHttpClientGenerator(JacksonModelGenerator)
  implicit def EnumProtocolInterp    = JacksonProtocolGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = new AkkaHttpGenerator(JacksonModelGenerator)
  implicit def ModelProtocolInterp   = JacksonProtocolGenerator.ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = JacksonProtocolGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = JacksonProtocolGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = new AkkaHttpServerGenerator(JacksonModelGenerator)
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaGenerator
}
