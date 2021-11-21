package dev.guardrail.generators.scala.http4s

import dev.guardrail.Target
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.scala.{ CirceModelGenerator, ScalaLanguage }
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.ScalaCollectionsGenerator

class Http4s(version: Http4sVersion) extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
  implicit def ProtocolInterp       = CirceProtocolGenerator(CirceModelGenerator.V012)
  implicit def ClientInterp         = Http4sClientGenerator()
  implicit def FrameworkInterp      = Http4sGenerator()
  implicit def ServerInterp         = Http4sServerGenerator(version)
  implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]()
  implicit def LanguageInterp       = ScalaGenerator()
}
