package dev.guardrail.terms.server

import cats.data.NonEmptyList

import dev.guardrail._
import dev.guardrail.languages.LA
import dev.guardrail.terms.Responses
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, RouteMeta, SecurityScheme, SwaggerTerms }
import dev.guardrail.core.Tracker
import io.swagger.v3.oas.models.Components
import dev.guardrail.generators._

case class GenerateRouteMeta[L <: LA](
    operationId: String,
    methodName: String,
    responseClsName: String,
    customExtractionField: Option[CustomExtractionField[L]],
    tracingField: Option[TracingField[L]],
    routeMeta: RouteMeta,
    parameters: LanguageParameters[L],
    responses: Responses[L]
)

sealed trait SecurityExposure
object SecurityExposure {
  case object Undefined extends SecurityExposure
  case object Required  extends SecurityExposure
  case object Optional  extends SecurityExposure
}

abstract class ServerTerms[L <: LA, F[_]] { self =>
  def fromSpec(context: Context, supportPackage: NonEmptyList[String], basePath: Option[String], frameworkImports: List[L#Import])(
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[L]],
      securitySchemes: Map[String, SecurityScheme[L]],
      components: Tracker[Option[Components]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F]): F[Servers[L]]
}
