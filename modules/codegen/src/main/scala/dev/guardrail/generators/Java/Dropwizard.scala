package dev.guardrail.generators.Java

import dev.guardrail.Target
import dev.guardrail.generators.Java.AsyncHttpClientClientGenerator.ClientTermInterp
import dev.guardrail.generators.Java.DropwizardGenerator.{ FrameworkInterp => DropwizardFrameworkInterp }
import dev.guardrail.generators.Java.DropwizardServerGenerator.ServerTermInterp
import dev.guardrail.generators.Java.JacksonGenerator._
import dev.guardrail.generators.JavaGenerator.JavaInterp
import dev.guardrail.generators.collections.JavaCollectionsGenerator.JavaCollectionsInterp
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.languages.JavaLanguage
import dev.guardrail.terms.collections.JavaStdLibCollections

object Dropwizard extends Framework[JavaLanguage, Target] {
  private implicit val col = JavaStdLibCollections

  implicit def CollectionsLibInterp  = new JavaCollectionsInterp
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
}
