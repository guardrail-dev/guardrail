package com.twilio.guardrail.tests

import scala.meta._
import com.twilio.guardrail.generators.GeneratorSettings

object defaults {
  val akkaGeneratorSettings   = new GeneratorSettings(t"BodyPartEntity", t"io.circe.Json")
  val http4sGeneratorSettings = new GeneratorSettings(t"java.io.File", t"io.circe.Json")
}
