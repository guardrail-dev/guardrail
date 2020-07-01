package com.twilio.guardrail.generators.Scala

import com.twilio.guardrail.Target
import com.twilio.guardrail.generators.{ Framework, SwaggerGenerator }
import com.twilio.guardrail.generators.Scala.model.CirceModelGenerator
import com.twilio.guardrail.generators.ScalaGenerator._
import com.twilio.guardrail.languages.ScalaLanguage
import Http4sClientGenerator._
import Http4sServerGenerator._
import Http4sGenerator.{ FrameworkInterp => Http4sFrameworkInterp }
import CirceProtocolGenerator._
import com.twilio.guardrail.generators.collections.ScalaCollectionsGenerator.ScalaCollectionsInterp

object Http4s extends Framework[ScalaLanguage, Target] {
  implicit def CollectionsLibInterp  = ScalaCollectionsInterp
  implicit def ArrayProtocolInterp   = new ArrayProtocolTermInterp
  implicit def ClientInterp          = new ClientTermInterp
  implicit def EnumProtocolInterp    = new EnumProtocolTermInterp
  implicit def FrameworkInterp       = new Http4sFrameworkInterp
  implicit def ModelProtocolInterp   = new ModelProtocolTermInterp(CirceModelGenerator.V012)
  implicit def PolyProtocolInterp    = new PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = new ProtocolSupportTermInterp
  implicit def ServerInterp          = new ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[ScalaLanguage]
  implicit def LanguageInterp        = ScalaInterp
}
