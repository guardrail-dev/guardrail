package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.Scala.CirceProtocolGenerator._
import com.twilio.guardrail.generators.Scala.EndpointsClientGenerator._
import com.twilio.guardrail.generators.Scala.EndpointsGenerator.{ FrameworkInterp => EndpointsFrameworkInterp }
import com.twilio.guardrail.generators.Scala.EndpointsServerGenerator._
import com.twilio.guardrail.generators.Scala.model.CirceModelGenerator
import com.twilio.guardrail.generators.ScalaGenerator._
import com.twilio.guardrail.generators.collections.ScalaCollectionsGenerator.ScalaCollectionsInterp
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.languages.ScalaLanguage

object Endpoints extends Framework[ScalaLanguage, Target] {
  implicit def ArrayProtocolInterp   = ArrayProtocolTermInterp
  implicit def ClientInterp          = ClientTermInterp
  implicit def EnumProtocolInterp    = EnumProtocolTermInterp
  implicit def FrameworkInterp       = EndpointsFrameworkInterp
  implicit def ModelProtocolInterp   = new ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = ProtocolSupportTermInterp
  implicit def ServerInterp          = ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
  implicit def CollectionsLibInterp  = ScalaCollectionsInterp
}
