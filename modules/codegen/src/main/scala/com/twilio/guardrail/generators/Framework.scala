package com.twilio.guardrail
package generators

import cats.~>
import cats.arrow.FunctionK

import com.twilio.guardrail.languages.LA

import com.twilio.guardrail.protocol.terms.protocol.{ ArrayProtocolTerm, EnumProtocolTerm, ModelProtocolTerm, PolyProtocolTerm, ProtocolSupportTerm }
import com.twilio.guardrail.protocol.terms.client.ClientTerm
import com.twilio.guardrail.protocol.terms.server.ServerTerm
import com.twilio.guardrail.terms.{ ScalaTerm, SwaggerTerm }
import com.twilio.guardrail.terms.framework.FrameworkTerm

trait Framework[L <: LA, F[_]] extends FunctionK[CodegenApplication[L, ?], F] {
  implicit def ArrayProtocolInterp: FunctionK[ArrayProtocolTerm[L, ?], F]
  implicit def ClientInterp: FunctionK[ClientTerm[L, ?], F]
  implicit def EnumProtocolInterp: FunctionK[EnumProtocolTerm[L, ?], F]
  implicit def FrameworkInterp: FunctionK[FrameworkTerm[L, ?], F]
  implicit def ModelProtocolInterp: FunctionK[ModelProtocolTerm[L, ?], F]
  implicit def PolyProtocolInterp: FunctionK[PolyProtocolTerm[L, ?], F]
  implicit def ProtocolSupportInterp: FunctionK[ProtocolSupportTerm[L, ?], F]
  implicit def ServerInterp: FunctionK[ServerTerm[L, ?], F]
  implicit def SwaggerInterp: FunctionK[SwaggerTerm[L, ?], F]
  implicit def LanguageInterp: FunctionK[ScalaTerm[L, ?], F]

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
