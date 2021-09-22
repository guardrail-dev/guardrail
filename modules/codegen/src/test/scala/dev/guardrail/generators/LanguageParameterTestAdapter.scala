package dev.guardrail

import dev.guardrail.generators.scala.ScalaLanguage

package object generators {
  implicit class RichLanguageParameter(value: LanguageParameter[ScalaLanguage]) {
    def withRawName(rawParameterName: String, rawType: Option[String] = Some("string"), rawFormat: Option[String] = None): LanguageParameter[ScalaLanguage] =
      new LanguageParameter[ScalaLanguage](
        value.in,
        value.param,
        value.paramName,
        RawParameterName(rawParameterName),
        value.argType,
        RawParameterType(rawType, rawFormat),
        value.required,
        value.hashAlgorithm,
        value.isFile
      )
  }
}
