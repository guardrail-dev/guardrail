package dev.guardrail.generators.java.springMvc

import _root_.io.swagger.v3.oas.models.Components
import cats.data.NonEmptyList
import dev.guardrail._
import dev.guardrail.core.Tracker
import dev.guardrail.generators.Clients
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.terms._
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.StrictProtocolElems

import java.net.URI

object SpringMvcClientGenerator {
  def apply()(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]): ClientTerms[JavaLanguage, Target] =
    new SpringMvcClientGenerator
}

class SpringMvcClientGenerator private (implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends ClientTerms[JavaLanguage, Target] {
  override def fromSpec(context: Context, frameworkImports: List[JavaLanguage#Import])(
      serverUrls: Option[NonEmptyList[URI]],
      basePath: Option[String],
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[JavaLanguage]],
      securitySchemes: Map[String, SecurityScheme[JavaLanguage]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: OpenAPITerms[JavaLanguage, Target]
  ): Target[Clients[JavaLanguage]] =
    Target.raiseUserError("spring client generation is not currently supported")
}
