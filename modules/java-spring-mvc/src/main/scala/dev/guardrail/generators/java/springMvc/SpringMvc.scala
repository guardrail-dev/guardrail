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

  implicit def CollectionsLibInterp  = new JavaCollectionsGenerator
  implicit def ArrayProtocolInterp   = new JacksonGenerator.ArrayProtocolTermInterp
  implicit def ClientInterp          = new SpringMvcClientGenerator
  implicit def EnumProtocolInterp    = new JacksonGenerator.EnumProtocolTermInterp
  implicit def FrameworkInterp       = new SpringMvcGenerator
  implicit def ModelProtocolInterp   = new JacksonGenerator.ModelProtocolTermInterp
  implicit def PolyProtocolInterp    = new JacksonGenerator.PolyProtocolTermInterp
  implicit def ProtocolSupportInterp = new JacksonGenerator.ProtocolSupportTermInterp
  implicit def ServerInterp          = new SpringMvcServerGenerator
  implicit def SwaggerInterp         = SwaggerGenerator[JavaLanguage]
  implicit def LanguageInterp        = JavaGenerator
}
