package com.twilio.guardrail
package generators
package Java

import com.twilio.guardrail.languages.JavaLanguage
import cats.~>

object AkkaHttp extends (CodegenApplication[JavaLanguage, ?] ~> Target) {
  /*
  val interpDefinitionPM: DefinitionPM[JavaLanguage, ?] ~> Target       = ProtocolSupportTermInterp or ModelProtocolTermInterp
  val interpDefinitionPME: DefinitionPME[JavaLanguage, ?] ~> Target     = EnumProtocolTermInterp or interpDefinitionPM
  val interpDefinitionPMEA: DefinitionPMEA[JavaLanguage, ?] ~> Target   = ArrayProtocolTermInterp or interpDefinitionPME
  val interpDefinitionPMEAP: DefinitionPMEAP[JavaLanguage, ?] ~> Target = PolyProtocolTermInterp or interpDefinitionPMEA

  val interpModel: ModelInterpreters[JavaLanguage, ?] ~> Target = interpDefinitionPMEAP

  val interpFrameworkC: FrameworkC[JavaLanguage, ?] ~> Target     = ClientTermInterp or interpModel
  val interpFrameworkCS: FrameworkCS[JavaLanguage, ?] ~> Target   = ServerTermInterp or interpFrameworkC
  val interpFrameworkCSF: FrameworkCSF[JavaLanguage, ?] ~> Target = FrameworkInterp or interpFrameworkCS

  val interpFramework: ClientServerTerms[JavaLanguage, ?] ~> Target = interpFrameworkCSF

  val parser: Parser[JavaLanguage, ?] ~> Target = SwaggerInterp or interpFramework

  val codegenApplication: CodegenApplication[JavaLanguage, ?] ~> Target = ScalaInterp or parser
  */

  def apply[T](x: CodegenApplication[JavaLanguage, T]): Target[T] = Target.raiseError(x.toString())
}
