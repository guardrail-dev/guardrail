package com.twilio.guardrail

import com.twilio.guardrail.languages.ScalaLanguage

package object generators {
  implicit class RichScalaParameter(value: ScalaParameter[ScalaLanguage]) {
    def withRawName(rawParameterName: String): ScalaParameter[ScalaLanguage] =
      new ScalaParameter[ScalaLanguage](value.in,
                                        value.param,
                                        value.paramName,
                                        RawParameterName(rawParameterName),
                                        value.argType,
                                        value.required,
                                        value.hashAlgorithm,
                                        value.isFile)
  }
}
