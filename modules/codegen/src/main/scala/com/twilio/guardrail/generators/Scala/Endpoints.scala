package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.circe.CirceVersion
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.generators.ScalaGenerator._
import com.twilio.guardrail.languages.ScalaLanguage

import EndpointsClientGenerator._
import EndpointsServerGenerator._
import CirceProtocolGenerator._
import EndpointsGenerator.{ FrameworkInterp => EndpointsFrameworkInterp }

object Endpoints extends Framework[ScalaLanguage, Target] {
  implicit def ArrayProtocolInterp   = ArrayProtocolTermInterp
  implicit def ClientInterp          = ClientTermInterp
  implicit def EnumProtocolInterp    = EnumProtocolTermInterp
  implicit def FrameworkInterp       = EndpointsFrameworkInterp
  implicit def ModelProtocolInterp   = new ModelProtocolTermInterp(CirceVersion.V012)
  implicit def PolyProtocolInterp    = PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = ProtocolSupportTermInterp
  implicit def ServerInterp          = ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
