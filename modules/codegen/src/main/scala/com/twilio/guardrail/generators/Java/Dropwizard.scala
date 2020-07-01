package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.Java.AsyncHttpClientClientGenerator.ClientTermInterp
import com.twilio.guardrail.generators.Java.DropwizardGenerator.{ FrameworkInterp => DropwizardFrameworkInterp }
import com.twilio.guardrail.generators.Java.DropwizardServerGenerator.ServerTermInterp
import com.twilio.guardrail.generators.Java.JacksonGenerator._
import com.twilio.guardrail.generators.JavaGenerator.JavaInterp
import com.twilio.guardrail.generators.collections.JavaCollectionsGenerator.JavaCollectionsInterp
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.JavaLanguage

object Dropwizard extends Framework[JavaLanguage, Target] {
  implicit def ArrayProtocolInterp   = ArrayProtocolTermInterp
  implicit def ClientInterp          = ClientTermInterp
  implicit def EnumProtocolInterp    = EnumProtocolTermInterp
  implicit def FrameworkInterp       = DropwizardFrameworkInterp
  implicit def ModelProtocolInterp   = ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = ProtocolSupportTermInterp
  implicit def ServerInterp          = ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp        = JavaInterp
  implicit def CollectionsLibInterp  = JavaCollectionsInterp
}
