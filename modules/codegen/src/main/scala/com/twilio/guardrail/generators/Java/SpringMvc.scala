package com.twilio.guardrail.generators.Java

import cats.~>
import com.twilio.guardrail.generators.Java.SpringMvcClientGenerator.ClientTermInterp
import com.twilio.guardrail.generators.Java.SpringMvcGenerator.FrameworkInterp
import com.twilio.guardrail.generators.Java.SpringMvcServerGenerator.ServerTermInterp
import com.twilio.guardrail.generators.Java.JacksonGenerator.{
  ArrayProtocolTermInterp,
  EnumProtocolTermInterp,
  ModelProtocolTermInterp,
  PolyProtocolTermInterp,
  ProtocolSupportTermInterp
}
import com.twilio.guardrail.generators.JavaGenerator.JavaInterp
import com.twilio.guardrail.generators.SwaggerGenerator
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.{
  ClientServerTerms,
  CodegenApplication,
  DefinitionPM,
  DefinitionPME,
  DefinitionPMEA,
  DefinitionPMEAP,
  FrameworkC,
  FrameworkCS,
  FrameworkCSF,
  ModelInterpreters,
  Parser,
  Target
}

object SpringMvc extends (CodegenApplication[JavaLanguage, ?] ~> Target) {
  val interpDefinitionPM: DefinitionPM[JavaLanguage, ?] ~> Target       = ProtocolSupportTermInterp or ModelProtocolTermInterp
  val interpDefinitionPME: DefinitionPME[JavaLanguage, ?] ~> Target     = EnumProtocolTermInterp or interpDefinitionPM
  val interpDefinitionPMEA: DefinitionPMEA[JavaLanguage, ?] ~> Target   = ArrayProtocolTermInterp or interpDefinitionPME
  val interpDefinitionPMEAP: DefinitionPMEAP[JavaLanguage, ?] ~> Target = PolyProtocolTermInterp or interpDefinitionPMEA

  val interpModel: ModelInterpreters[JavaLanguage, ?] ~> Target = interpDefinitionPMEAP

  val interpFrameworkC: FrameworkC[JavaLanguage, ?] ~> Target     = ClientTermInterp or interpModel
  val interpFrameworkCS: FrameworkCS[JavaLanguage, ?] ~> Target   = ServerTermInterp or interpFrameworkC
  val interpFrameworkCSF: FrameworkCSF[JavaLanguage, ?] ~> Target = FrameworkInterp or interpFrameworkCS

  val interpFramework: ClientServerTerms[JavaLanguage, ?] ~> Target = interpFrameworkCSF

  val parser: Parser[JavaLanguage, ?] ~> Target = SwaggerGenerator[JavaLanguage] or interpFramework

  val codegenApplication: CodegenApplication[JavaLanguage, ?] ~> Target = JavaInterp or parser

  def apply[T](x: CodegenApplication[JavaLanguage, T]): Target[T] = codegenApplication.apply(x)
}
