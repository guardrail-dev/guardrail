package com.twilio.guardrail
package generators

import com.twilio.guardrail.circe.CirceVersion
import com.twilio.guardrail.languages.ScalaLanguage

import Http4sClientGenerator._
import Http4sServerGenerator._
import Http4sGenerator.{ FrameworkInterp => Http4sFrameworkInterp }
import CirceProtocolGenerator._
import ScalaGenerator._

object Http4s extends Framework[ScalaLanguage, Target] {
  implicit def ArrayProtocolInterp   = ArrayProtocolTermInterp
  implicit def ClientInterp          = ClientTermInterp
  implicit def EnumProtocolInterp    = EnumProtocolTermInterp
  implicit def FrameworkInterp       = Http4sFrameworkInterp
  implicit def ModelProtocolInterp   = new ModelProtocolTermInterp(CirceVersion.V012)
  implicit def PolyProtocolInterp    = PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = ProtocolSupportTermInterp
  implicit def ServerInterp          = ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
