package com.twilio.guardrail.generators.Java

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.JavaGenerator.JavaInterp
import com.twilio.guardrail.generators.collections.JavaCollectionsGenerator.JavaCollectionsInterp
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.terms.collections.JavaStdLibCollections

object SpringMvc extends Framework[JavaLanguage, Target] {
  private implicit val col = JavaStdLibCollections

  implicit def CollectionsLibInterp  = new JavaCollectionsInterp
  implicit def ArrayProtocolInterp   = JacksonGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = SpringMvcClientGenerator.ClientTermInterp
  implicit def EnumProtocolInterp    = JacksonGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = SpringMvcGenerator.FrameworkInterp
  implicit def ModelProtocolInterp   = JacksonGenerator.ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = JacksonGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = JacksonGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = SpringMvcServerGenerator.ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp        = JavaInterp
}
