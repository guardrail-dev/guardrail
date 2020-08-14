package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.Scala.AkkaHttpClientGenerator._
import com.twilio.guardrail.generators.Scala.AkkaHttpGenerator._
import com.twilio.guardrail.generators.Scala.AkkaHttpServerGenerator._
import com.twilio.guardrail.generators.Scala.CirceProtocolGenerator._
import com.twilio.guardrail.generators.Scala.model.CirceModelGenerator
import com.twilio.guardrail.generators.ScalaGenerator._
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.ScalaLanguage

object AkkaHttp extends Framework[ScalaLanguage, Target] {
  implicit def ArrayProtocolInterp   = ArrayProtocolTermInterp
  implicit def ClientInterp          = new ClientTermInterp(CirceModelGenerator.V012)
  implicit def EnumProtocolInterp    = EnumProtocolTermInterp
  implicit def FrameworkInterp       = new FrameworkInterp(CirceModelGenerator.V012)
  implicit def ModelProtocolInterp   = new ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = ProtocolSupportTermInterp
  implicit def ServerInterp          = new ServerTermInterp(CirceModelGenerator.V012)
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
