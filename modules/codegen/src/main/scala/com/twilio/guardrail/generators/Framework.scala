package com.twilio.guardrail
package generators

import cats.~>
import cats.arrow.FunctionK

import com.twilio.guardrail.languages.LA

import com.twilio.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.terms.{ LanguageTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms

trait Framework[L <: LA, F[_]] extends FunctionK[CodegenApplication[L, ?], F] {
  implicit def ArrayProtocolInterp: ArrayProtocolTerms[L, F]
  implicit def ClientInterp: ClientTerms[L, F]
  implicit def EnumProtocolInterp: EnumProtocolTerms[L, F]
  implicit def FrameworkInterp: FrameworkTerms[L, F]
  implicit def ModelProtocolInterp: ModelProtocolTerms[L, F]
  implicit def PolyProtocolInterp: PolyProtocolTerms[L, F]
  implicit def ProtocolSupportInterp: ProtocolSupportTerms[L, F]
  implicit def ServerInterp: ServerTerms[L, F]
  implicit def SwaggerInterp: SwaggerTerms[L, F]
  implicit def LanguageInterp: LanguageTerms[L, F]

  val interpDefinitionPM: DefinitionPM[L, ?] ~> F       = ProtocolSupportInterp or ModelProtocolInterp
  val interpDefinitionPME: DefinitionPME[L, ?] ~> F     = EnumProtocolInterp or interpDefinitionPM
  val interpDefinitionPMEA: DefinitionPMEA[L, ?] ~> F   = ArrayProtocolInterp or interpDefinitionPME
  val interpDefinitionPMEAP: DefinitionPMEAP[L, ?] ~> F = PolyProtocolInterp or interpDefinitionPMEA

  val interpModel: ModelInterpreters[L, ?] ~> F = interpDefinitionPMEAP

  val interpFrameworkC: FrameworkC[L, ?] ~> F     = ClientInterp or interpModel
  val interpFrameworkCS: FrameworkCS[L, ?] ~> F   = ServerInterp or interpFrameworkC
  val interpFrameworkCSF: FrameworkCSF[L, ?] ~> F = FrameworkInterp or interpFrameworkCS

  val interpFramework: ClientServerTerms[L, ?] ~> F = interpFrameworkCSF

  val parser: Parser[L, ?] ~> F = SwaggerInterp or interpFramework

  val codegenApplication: CodegenApplication[L, ?] ~> F = LanguageInterp or parser

  def apply[T](x: CodegenApplication[L, T]): F[T] = codegenApplication.apply(x)
}
