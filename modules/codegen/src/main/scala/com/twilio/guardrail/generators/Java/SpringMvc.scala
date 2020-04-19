package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.generators.Framework
import com.twilio.guardrail.generators.Java.SpringMvcClientGenerator.ClientTermInterp
import com.twilio.guardrail.generators.Java.SpringMvcGenerator.{ FrameworkInterp => FrameworkTermInterp }
import com.twilio.guardrail.generators.Java.SpringMvcServerGenerator.ServerTermInterp
import com.twilio.guardrail.generators.Java.JacksonGenerator.{
  ArrayProtocolTermInterp,
  EnumProtocolTermInterp,
  ModelProtocolTermInterp,
  PolyProtocolTermInterp,
  ProtocolSupportTermInterp
}
import com.twilio.guardrail.generators.JavaGenerator.JavaInterp
import com.twilio.guardrail.generators.SwaggerGenerator
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.Target

object SpringMvc extends Framework[JavaLanguage, Target] {
  implicit def ArrayProtocolInterp   = ArrayProtocolTermInterp
  implicit def ClientInterp          = ClientTermInterp
  implicit def EnumProtocolInterp    = EnumProtocolTermInterp
  implicit def FrameworkInterp       = FrameworkTermInterp
  implicit def ModelProtocolInterp   = ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = ProtocolSupportTermInterp
  implicit def ServerInterp          = ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp        = JavaInterp
}
