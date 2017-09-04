package com.twilio.swagger.codegen
package generators

import cats.~>
import cats.arrow.FunctionK

import AkkaHttpClientGenerator._
import AkkaHttpServerGenerator._
import CirceProtocolGenerator._
import ScalaGenerator._

object AkkaHttp extends FunctionK[CodegenApplication, Target] {
  val interpSP: CodegenApplicationSP ~> Target = ProtocolSupportTermInterp or ServerTermInterp
  val interpMSP: CodegenApplicationMSP ~> Target = ModelProtocolTermInterp or interpSP
  val interpEMSP: CodegenApplicationEMSP ~> Target = EnumProtocolTermInterp or interpMSP
  val interpCEMSP: CodegenApplicationCEMSP ~> Target = ClientTermInterp or interpEMSP
  val interpACEMSP: CodegenApplicationACEMSP ~> Target = AliasProtocolTermInterp or interpCEMSP
  val interpACEMSSP: CodegenApplicationACEMSSP ~> Target = ScalaInterp or interpACEMSP
  def apply[T](x: CodegenApplication[T]): Target[T] = interpACEMSSP.apply(x)
}
