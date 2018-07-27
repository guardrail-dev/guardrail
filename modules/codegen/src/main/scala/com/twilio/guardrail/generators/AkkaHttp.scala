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
  val interpSP: CodegenApplicationSP ~> Target                 = ProtocolSupportTermInterp or ServerTermInterp
  val interpMSP: CodegenApplicationMSP ~> Target               = ModelProtocolTermInterp or interpSP
  val interpEMSP: CodegenApplicationEMSP ~> Target             = EnumProtocolTermInterp or interpMSP
  val interpCEMSP: CodegenApplicationCEMSP ~> Target           = ClientTermInterp or interpEMSP
  val interpACEMSP: CodegenApplicationACEMSP ~> Target         = AliasProtocolTermInterp or interpCEMSP
  val interpACEMSSP: CodegenApplicationACEMSSP ~> Target       = ScalaInterp or interpACEMSP
  val interpACEMSSPR: CodegenApplicationACEMSSPR ~> Target     = ArrayProtocolTermInterp or interpACEMSSP
  val interpACEMSSPRS: CodegenApplicationACEMSSPRS ~> Target   = SwaggerInterp or interpACEMSSPR
  val interpACEMSSPRSF: CodegenApplicationACEMSSPRSF ~> Target = FrameworkInterp or interpACEMSSPRS

  def apply[T](x: CodegenApplication[T]): Target[T] = interpACEMSSPRSF.apply(x)
}
