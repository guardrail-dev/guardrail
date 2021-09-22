package dev.guardrail.generators.java.dropwizard

import dev.guardrail.Target
import dev.guardrail.generators.java.asyncHttpClient.AsyncHttpClientClientGenerator.ClientTermInterp
import dev.guardrail.generators.java.dropwizard.DropwizardGenerator.{ FrameworkInterp => DropwizardFrameworkInterp }
import dev.guardrail.generators.java.dropwizard.DropwizardServerGenerator.ServerTermInterp
import dev.guardrail.generators.java.jackson.JacksonGenerator
import dev.guardrail.generators.java.JavaGenerator.JavaInterp
import dev.guardrail.generators.collections.JavaCollectionsGenerator.JavaCollectionsInterp
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.terms.collections.JavaStdLibCollections

object Dropwizard extends Framework[JavaLanguage, Target] {
  private implicit val col = JavaStdLibCollections

  implicit def CollectionsLibInterp  = new JavaCollectionsInterp
  implicit def ArrayProtocolInterp   = JacksonGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = ClientTermInterp
  implicit def EnumProtocolInterp    = JacksonGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = DropwizardFrameworkInterp
  implicit def ModelProtocolInterp   = JacksonGenerator.ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = JacksonGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = JacksonGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = ServerTermInterp
  implicit def SwaggerInterp         = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp        = JavaInterp
}
