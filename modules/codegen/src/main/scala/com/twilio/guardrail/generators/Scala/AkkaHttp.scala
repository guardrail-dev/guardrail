package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.Scala.model.CirceModelGenerator
import com.twilio.guardrail.generators.ScalaGenerator._
import com.twilio.guardrail.generators.collections.ScalaCollectionsGenerator.ScalaCollectionsInterp
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.ScalaLanguage

object AkkaHttp extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsInterp
  implicit def ArrayProtocolInterp   = CirceProtocolGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = AkkaHttpClientGenerator.ClientTermInterp(CirceModelGenerator.V012)
  implicit def EnumProtocolInterp    = CirceProtocolGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = AkkaHttpGenerator.FrameworkInterp(CirceModelGenerator.V012)
  implicit def ModelProtocolInterp   = CirceProtocolGenerator.ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = CirceProtocolGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = CirceProtocolGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = AkkaHttpServerGenerator.ServerTermInterp(CirceModelGenerator.V012)
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
