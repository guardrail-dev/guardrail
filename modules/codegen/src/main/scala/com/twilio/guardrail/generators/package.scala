package com.twilio.guardrail.generators.syntax
import com.twilio.guardrail.generators._
import _root_.scala.meta._

object scala {
  implicit class RichRawParameterName(parameter: RawParameterName) {
    def toLit: Lit.String = Lit.String(parameter.value)
  }
}
