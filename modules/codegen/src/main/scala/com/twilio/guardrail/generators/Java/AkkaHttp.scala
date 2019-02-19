package com.twilio.guardrail
package generators
package Java

import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.client.ClientTerm
import com.twilio.guardrail.protocol.terms.server.ServerTerm
import com.twilio.guardrail.terms.framework.FrameworkTerm
import cats.~>

import JacksonProtocolGenerator._
import JavaGenerator.JavaInterp

object AkkaHttp extends (CodegenApplication[JavaLanguage, ?] ~> Target) {
  object ClientTermInterp extends (ClientTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ClientTerm[JavaLanguage, T]): Target[T] = Target.raiseError(s"interpFramework: ${term.toString()}")
  }
  object FrameworkInterp extends (FrameworkTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: FrameworkTerm[JavaLanguage, T]): Target[T] = Target.raiseError(s"interpFramework: ${term.toString()}")
  }
  object ServerTermInterp extends (ServerTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ServerTerm[JavaLanguage, T]): Target[T] = Target.raiseError(s"interpFramework: ${term.toString()}")
  }

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
