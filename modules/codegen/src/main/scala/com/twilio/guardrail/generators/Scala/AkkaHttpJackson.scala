package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.Scala.AkkaHttpClientGenerator._
import com.twilio.guardrail.generators.Scala.AkkaHttpGenerator._
import com.twilio.guardrail.generators.Scala.AkkaHttpServerGenerator._
import com.twilio.guardrail.generators.Scala.JacksonProtocolGenerator._
import com.twilio.guardrail.generators.Scala.model.JacksonModelGenerator
import com.twilio.guardrail.generators.ScalaGenerator._
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.ScalaLanguage

object AkkaHttpJackson extends Framework[ScalaLanguage, Target] {
  implicit def ArrayProtocolInterp   = ArrayProtocolTermInterp
  implicit def ClientInterp          = new ClientTermInterp(JacksonModelGenerator)
  implicit def EnumProtocolInterp    = EnumProtocolTermInterp
  implicit def FrameworkInterp       = new FrameworkInterp(JacksonModelGenerator)
  implicit def ModelProtocolInterp   = ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = ProtocolSupportTermInterp
  implicit def ServerInterp          = new ServerTermInterp(JacksonModelGenerator)
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
