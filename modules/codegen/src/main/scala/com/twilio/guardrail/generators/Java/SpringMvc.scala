package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.Java.JacksonGenerator._
import com.twilio.guardrail.generators.Java.SpringMvcClientGenerator.ClientTermInterp
import com.twilio.guardrail.generators.Java.SpringMvcGenerator.{ FrameworkInterp => FrameworkTermInterp }
import com.twilio.guardrail.generators.Java.SpringMvcServerGenerator.ServerTermInterp
import com.twilio.guardrail.generators.JavaGenerator.JavaInterp
import com.twilio.guardrail.generators.collections.JavaCollectionsGenerator.JavaCollectionsInterp
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.JavaLanguage

object SpringMvc extends Framework[JavaLanguage, Target] {
  implicit def CollectionsLibInterp  = JavaCollectionsInterp
  implicit def ArrayProtocolInterp   = new ArrayProtocolTermInterp
  implicit def ClientInterp          = new ClientTermInterp
  implicit def EnumProtocolInterp    = new EnumProtocolTermInterp
  implicit def FrameworkInterp       = new FrameworkTermInterp
  implicit def ModelProtocolInterp   = new ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = new PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = new ProtocolSupportTermInterp
  implicit def ServerInterp          = new ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp        = JavaInterp
}
