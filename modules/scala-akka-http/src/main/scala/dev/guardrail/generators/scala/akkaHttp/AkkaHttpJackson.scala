package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.{ JacksonModelGenerator, ScalaCollectionsGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.jackson.JacksonProtocolGenerator
import dev.guardrail.generators.scala.ScalaGenerator._
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttpJackson extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsGenerator.ScalaCollectionsInterp
  implicit def ArrayProtocolInterp   = JacksonProtocolGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = AkkaHttpClientGenerator.ClientTermInterp(JacksonModelGenerator)
  implicit def EnumProtocolInterp    = JacksonProtocolGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = AkkaHttpGenerator.FrameworkInterp(JacksonModelGenerator)
  implicit def ModelProtocolInterp   = JacksonProtocolGenerator.ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = JacksonProtocolGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = JacksonProtocolGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = AkkaHttpServerGenerator.ServerTermInterp(JacksonModelGenerator)
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
