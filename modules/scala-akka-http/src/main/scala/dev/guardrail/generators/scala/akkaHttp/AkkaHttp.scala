package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.{ AkkaHttpVersion, CirceModelGenerator, ScalaCollectionsGenerator, ScalaGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttp extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
  implicit def ClientInterp         = AkkaHttpClientGenerator(CirceModelGenerator.V012)
  implicit def FrameworkInterp      = AkkaHttpGenerator(AkkaHttpVersion.V10_2, CirceModelGenerator.V012)
  implicit def ProtocolInterp       = CirceProtocolGenerator(CirceModelGenerator.V012)
  implicit def ServerInterp         = AkkaHttpServerGenerator(AkkaHttpVersion.V10_2, CirceModelGenerator.V012)
  implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp       = ScalaGenerator()
}
