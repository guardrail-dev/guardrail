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

  implicit def CollectionsLibInterp = new JavaCollectionsGenerator
  implicit def ProtocolInterp       = new JacksonGenerator
  implicit def ClientInterp         = new AsyncHttpClientClientGenerator
  implicit def FrameworkInterp      = new DropwizardGenerator
  implicit def ServerInterp         = new DropwizardServerGenerator
  implicit def SwaggerInterp        = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp       = JavaGenerator
}
