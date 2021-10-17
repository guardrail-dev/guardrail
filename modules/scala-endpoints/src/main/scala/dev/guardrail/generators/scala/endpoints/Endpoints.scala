package dev.guardrail.generators.scala.endpoints

import dev.guardrail.Target
import dev.guardrail.generators.scala.ScalaCollectionsGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.CirceModelGenerator
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }

object Endpoints extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsGenerator
  implicit def ArrayProtocolInterp   = new CirceProtocolGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = new EndpointsClientGenerator
  implicit def EnumProtocolInterp    = new CirceProtocolGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = new EndpointsGenerator
  implicit def ModelProtocolInterp   = new CirceProtocolGenerator.ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = new CirceProtocolGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = new CirceProtocolGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = new EndpointsServerGenerator
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaGenerator
}
