package dev.guardrail.generators.java.dropwizard

import dev.guardrail.Target
import dev.guardrail.generators.java.asyncHttpClient.AsyncHttpClientClientGenerator
import dev.guardrail.generators.java.jackson.JacksonGenerator
import dev.guardrail.generators.java.JavaGenerator
import dev.guardrail.generators.collections.JavaCollectionsGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.terms.collections.JavaStdLibCollections

object Dropwizard extends Framework[JavaLanguage, Target] {
  private implicit val col = JavaStdLibCollections

  implicit def CollectionsLibInterp  = new JavaCollectionsGenerator
  implicit def ArrayProtocolInterp   = new JacksonGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = new AsyncHttpClientClientGenerator
  implicit def EnumProtocolInterp    = new JacksonGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = new DropwizardGenerator
  implicit def ModelProtocolInterp   = new JacksonGenerator.ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = new JacksonGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = new JacksonGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = new DropwizardServerGenerator
  implicit def SwaggerInterp         = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp        = JavaGenerator
}
