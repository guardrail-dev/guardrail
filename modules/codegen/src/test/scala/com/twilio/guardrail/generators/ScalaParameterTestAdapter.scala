package com.twilio.guardrail

import com.twilio.guardrail.languages.ScalaLanguage

package object generators {
  implicit class RichScalaParameter(value: ScalaParameter[ScalaLanguage]) {
    def withRawName(rawParameterName: String, rawType: String = "string", rawFormat: Option[String] = None): ScalaParameter[ScalaLanguage] =
      new ScalaParameter[ScalaLanguage](value.in,
                                        value.param,
                                        value.paramName,
                                        RawParameterName(rawParameterName),
                                        value.argType,
                                        RawParameterType(rawType, rawFormat),
                                        value.required,
                                        value.hashAlgorithm,
                                        value.isFile)
  }
}
