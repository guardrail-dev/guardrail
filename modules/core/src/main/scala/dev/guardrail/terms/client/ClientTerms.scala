package dev.guardrail.terms.client

import cats.data.NonEmptyList
import dev.guardrail.Context
import dev.guardrail.core.Tracker
import dev.guardrail.generators.Clients
import dev.guardrail.languages.LA
import dev.guardrail.terms._
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.StrictProtocolElems
import io.swagger.v3.oas.models.Components

import java.net.URI

abstract class ClientTerms[L <: LA, F[_]] { self =>
  def fromSpec(context: Context, frameworkImports: List[L#Import])(
      serverUrls: Option[NonEmptyList[URI]],
      basePath: Option[String],
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[L]],
      securitySchemes: Map[String, SecurityScheme[L]],
      components: Tracker[Option[Components]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: OpenAPITerms[L, F]): F[Clients[L]]
}
