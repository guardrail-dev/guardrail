package dev.guardrail.generators.scala.http4s

import dev.guardrail.Target
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.scala.{ CirceModelGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.circe.{ CirceProtocolGenerator }
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaCollectionsGenerator

object Http4s extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsGenerator
  implicit def ArrayProtocolInterp   = new CirceProtocolGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = new Http4sClientGenerator
  implicit def EnumProtocolInterp    = new CirceProtocolGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = new Http4sGenerator
  implicit def ModelProtocolInterp   = new CirceProtocolGenerator.ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = new CirceProtocolGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = new CirceProtocolGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = new Http4sServerGenerator
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaGenerator
}
