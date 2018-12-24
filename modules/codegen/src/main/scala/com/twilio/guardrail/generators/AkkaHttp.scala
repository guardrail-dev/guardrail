package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.ScalaLanguage
import cats.~>

import AkkaHttpClientGenerator._
import AkkaHttpServerGenerator._
import CirceProtocolGenerator._
import ScalaGenerator._
import SwaggerGenerator._
import AkkaHttpGenerator._

object AkkaHttp extends (CodegenApplication[ScalaLanguage, ?] ~> Target) {
  val interpDefinitionPM: DefinitionPM[ScalaLanguage, ?] ~> Target       = ProtocolSupportTermInterp or ModelProtocolTermInterp
  val interpDefinitionPME: DefinitionPME[ScalaLanguage, ?] ~> Target     = EnumProtocolTermInterp or interpDefinitionPM
  val interpDefinitionPMEA: DefinitionPMEA[ScalaLanguage, ?] ~> Target   = ArrayProtocolTermInterp or interpDefinitionPME
  val interpDefinitionPMEAP: DefinitionPMEAP[ScalaLanguage, ?] ~> Target = PolyProtocolTermInterp or interpDefinitionPMEA

  val interpModel: ModelInterpreters[ScalaLanguage, ?] ~> Target = interpDefinitionPMEAP

  val interpFrameworkC: FrameworkC[ScalaLanguage, ?] ~> Target     = ClientTermInterp or interpModel
  val interpFrameworkCS: FrameworkCS[ScalaLanguage, ?] ~> Target   = ServerTermInterp or interpFrameworkC
  val interpFrameworkCSF: FrameworkCSF[ScalaLanguage, ?] ~> Target = FrameworkInterp or interpFrameworkCS

  val interpFramework: ClientServerTerms[ScalaLanguage, ?] ~> Target = interpFrameworkCSF

  val parser: Parser[ScalaLanguage, ?] ~> Target = SwaggerInterp or interpFramework

  val codegenApplication: CodegenApplication[ScalaLanguage, ?] ~> Target = ScalaInterp or parser

  def apply[T](x: CodegenApplication[ScalaLanguage, T]): Target[T] = codegenApplication.apply(x)
}
