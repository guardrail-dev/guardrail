package dev.guardrail.generators.scala.http4s

import dev.guardrail.Target
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.scala.{ CirceModelGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.circe.{ CirceProtocolGenerator }
import dev.guardrail.generators.scala.ScalaGenerator._
import dev.guardrail.generators.scala.ScalaCollectionsGenerator.ScalaCollectionsInterp

object Http4s extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsInterp
  implicit def ArrayProtocolInterp   = CirceProtocolGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = Http4sClientGenerator.ClientTermInterp
  implicit def EnumProtocolInterp    = CirceProtocolGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = Http4sGenerator.FrameworkInterp
  implicit def ModelProtocolInterp   = CirceProtocolGenerator.ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = CirceProtocolGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = CirceProtocolGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = Http4sServerGenerator.ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
