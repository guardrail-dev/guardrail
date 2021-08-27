package dev.guardrail.generators.Scala

import dev.guardrail.Target
import dev.guardrail.generators.collections.ScalaCollectionsGenerator
import dev.guardrail.generators.Scala.model.JacksonModelGenerator
import dev.guardrail.generators.ScalaGenerator._
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.languages.ScalaLanguage

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
