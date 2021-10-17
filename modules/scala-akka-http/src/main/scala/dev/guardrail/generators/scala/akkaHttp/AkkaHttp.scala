package dev.guardrail.generators.scala.akkaHttp

import dev.guardrail.Target
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.CirceModelGenerator
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object AkkaHttp extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp = ScalaCollectionsGenerator
  implicit def ClientInterp         = new AkkaHttpClientGenerator(CirceModelGenerator.V012)
  implicit def FrameworkInterp      = new AkkaHttpGenerator(CirceModelGenerator.V012)
  implicit def ProtocolInterp       = new CirceProtocolGenerator(CirceModelGenerator.V012)
  implicit def ServerInterp         = new AkkaHttpServerGenerator(CirceModelGenerator.V012)
  implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp       = ScalaGenerator
}
