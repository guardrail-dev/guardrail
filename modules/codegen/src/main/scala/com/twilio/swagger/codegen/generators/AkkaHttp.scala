package com.twilio.swagger.codegen
package generators

import cats.~>
import cats.arrow.FunctionK

import AkkaHttpClientGenerator._
import AkkaHttpServerGenerator._
import CirceProtocolGenerator._
import ScalaGenerator._
import SwaggerGenerator._

object AkkaHttp extends FunctionK[CodegenApplication, Target] {
  val interpSP: CodegenApplicationSP ~> Target = ProtocolSupportTermInterp or ServerTermInterp
  val interpMSP: CodegenApplicationMSP ~> Target = ModelProtocolTermInterp or interpSP
  val interpEMSP: CodegenApplicationEMSP ~> Target = EnumProtocolTermInterp or interpMSP
  val interpCEMSP: CodegenApplicationCEMSP ~> Target = ClientTermInterp or interpEMSP
  val interpACEMSP: CodegenApplicationACEMSP ~> Target = AliasProtocolTermInterp or interpCEMSP
  val interpACEMSSP: CodegenApplicationACEMSSP ~> Target = ScalaInterp or interpACEMSP
  val interpACEMSSPR: CodegenApplicationACEMSSPR ~> Target = ArrayProtocolTermInterp or interpACEMSSP
  val interpACEMSSPRS: CodegenApplicationACEMSSPRS ~> Target = SwaggerInterp or interpACEMSSPR
  def apply[T](x: CodegenApplication[T]): Target[T] = interpACEMSSPRS.apply(x)
}
