package com.twilio.guardrail

package object generators {
  implicit class RichScalaParameter(value: ScalaParameter) {
    def withRawName(rawParameterName: String): ScalaParameter =
      new ScalaParameter(value.in, value.param, value.paramName, RawParameterName(rawParameterName), value.argType, value.required, value.hashAlgorithm, value.isFile)
  }
}
