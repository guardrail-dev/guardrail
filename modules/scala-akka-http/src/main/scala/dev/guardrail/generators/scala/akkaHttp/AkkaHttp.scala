package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.ScalaCollectionsGenerator.ScalaCollectionsInterp
import dev.guardrail.generators.scala.ScalaGenerator._
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.CirceModelGenerator
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttp extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsInterp
  implicit def ArrayProtocolInterp   = CirceProtocolGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = AkkaHttpClientGenerator.ClientTermInterp(CirceModelGenerator.V012)
  implicit def EnumProtocolInterp    = CirceProtocolGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = AkkaHttpGenerator.FrameworkInterp(CirceModelGenerator.V012)
  implicit def ModelProtocolInterp   = CirceProtocolGenerator.ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = CirceProtocolGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = CirceProtocolGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = AkkaHttpServerGenerator.ServerTermInterp(CirceModelGenerator.V012)
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
