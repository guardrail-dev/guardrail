package dev.guardrail.generators.scala.endpoints

import dev.guardrail.Target
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.CirceModelGenerator
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object Endpoints extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
  implicit def ProtocolInterp       = CirceProtocolGenerator(CirceModelGenerator.V012)
  implicit def ClientInterp         = EndpointsClientGenerator()
  implicit def FrameworkInterp      = EndpointsGenerator()
  implicit def ServerInterp         = EndpointsServerGenerator()
  implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp       = ScalaGenerator()
}
