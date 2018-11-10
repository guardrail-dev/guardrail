package com.twilio.guardrail.tests

import scala.meta._
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.languages.ScalaLanguage

object defaults {
  val akkaGeneratorSettings   = new GeneratorSettings[ScalaLanguage](t"BodyPartEntity", t"io.circe.Json")
  val http4sGeneratorSettings = new GeneratorSettings[ScalaLanguage](t"java.io.File", t"io.circe.Json")
}
