package dev.guardrail.generators.scala.dropwizard

import dev.guardrail.Target
import dev.guardrail.generators.scala.ScalaGenerator
import dev.guardrail.generators.scala.jackson._
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.scala.{ ScalaCollectionsGenerator, ScalaLanguage }

object Dropwizard extends Framework[ScalaLanguage, Target] {
  override implicit def ClientInterp         = DropwizardClientGenerator()
  override implicit def FrameworkInterp      = DropwizardGenerator()
  override implicit def ProtocolInterp       = JacksonProtocolGenerator.apply
  override implicit def ServerInterp         = DropwizardServerGenerator()
  override implicit def SwaggerInterp        = SwaggerGenerator[ScalaLanguage]()
  override implicit def LanguageInterp       = ScalaGenerator()
  override implicit def CollectionsLibInterp = ScalaCollectionsGenerator()
}
