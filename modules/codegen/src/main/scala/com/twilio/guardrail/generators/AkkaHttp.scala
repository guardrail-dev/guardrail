package com.twilio.guardrail
package generators

import cats.~>
import cats.arrow.FunctionK

import AkkaHttpClientGenerator._
import AkkaHttpServerGenerator._
import CirceProtocolGenerator._
import ScalaGenerator._
import SwaggerGenerator._
import AkkaHttpGenerator._

object AkkaHttp extends FunctionK[CodegenApplication, Target] {
  val interpDefinitionPM: DefinitionPM ~> Target         = ProtocolSupportTermInterp or ModelProtocolTermInterp
  val interpDefinitionPME: DefinitionPME ~> Target       = EnumProtocolTermInterp or interpDefinitionPM
  val interpDefinitionPMEA: DefinitionPMEA ~> Target     = AliasProtocolTermInterp or interpDefinitionPME
  val interpDefinitionPMEAA: DefinitionPMEAA ~> Target   = ArrayProtocolTermInterp or interpDefinitionPMEA
  val interpDefinitionPMEAAP: DefinitionPMEAAP ~> Target = PolyProtocolTermInterp or interpDefinitionPMEAA

  val interpModel: ModelInterpreters ~> Target = interpDefinitionPMEAAP

  val interpFrameworkC: FrameworkC ~> Target     = ClientTermInterp or interpModel
  val interpFrameworkCS: FrameworkCS ~> Target   = ServerTermInterp or interpFrameworkC
  val interpFrameworkCSF: FrameworkCSF ~> Target = FrameworkInterp or interpFrameworkCS

  val interpFramework: ClientServerTerms ~> Target = interpFrameworkCSF

  val parser: Parser ~> Target = SwaggerInterp or interpFramework

  val codegenApplication: CodegenApplication ~> Target = ScalaInterp or parser

  def apply[T](x: CodegenApplication[T]): Target[T] = codegenApplication.apply(x)
}
