package dev.guardrail.generators.java.springMvc

import dev.guardrail.Target
import dev.guardrail.generators.java.JavaGenerator
import dev.guardrail.generators.java.jackson.JacksonGenerator
import dev.guardrail.generators.collections.JavaCollectionsGenerator
import dev.guardrail.generators.{ Framework, SwaggerGenerator }
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.terms.collections.JavaStdLibCollections

object SpringMvc extends Framework[JavaLanguage, Target] {
  private implicit val col = JavaStdLibCollections

  implicit def CollectionsLibInterp = JavaCollectionsGenerator()
  implicit def ProtocolInterp       = JacksonGenerator()
  implicit def ClientInterp         = SpringMvcClientGenerator()
  implicit def FrameworkInterp      = SpringMvcGenerator()
  implicit def ServerInterp         = SpringMvcServerGenerator()
  implicit def SwaggerInterp        = SwaggerGenerator[JavaLanguage]()
  implicit def LanguageInterp       = JavaGenerator()
}
