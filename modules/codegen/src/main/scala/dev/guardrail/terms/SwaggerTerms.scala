package dev.guardrail
package terms

import cats.Monad
import cats.data.NonEmptyList
import dev.guardrail.core.{ Mappish, Tracker }
import dev.guardrail.languages.LA
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media.{ ArraySchema, Schema }
import io.swagger.v3.oas.models.parameters.{ Parameter, RequestBody }
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }

abstract class SwaggerLogAdapter[F[_]] {
  def schemaToString(value: Schema[_]): String = "    " + value.toString().linesIterator.filterNot(_.contains(": null")).mkString("\n    ")
  def function[A](name: String): F[A] => F[A]
  def push(name: String): F[Unit]
  def pop: F[Unit]
  def debug(message: String): F[Unit]
  def info(message: String): F[Unit]
  def warning(message: String): F[Unit]
  def error(message: String): F[Unit]
}

abstract class SwaggerTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]

  def extractCommonRequestBodies(components: Tracker[Option[Components]]): F[Map[String, RequestBody]]
  def extractEnum(swagger: Tracker[EnumSchema]): F[Either[String, HeldEnum]]
  def extractOperations(
      paths: Tracker[Mappish[List, String, PathItem]],
      commonRequestBodies: Map[String, RequestBody],
      globalSecurityRequirements: Option[SecurityRequirements]
  ): F[List[RouteMeta]]
  def extractApiKeySecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): F[ApiKeySecurityScheme[L]]
  def extractHttpSecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): F[HttpSecurityScheme[L]]
  def extractOpenIdConnectSecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): F[OpenIdConnectSecurityScheme[L]]
  def extractOAuth2SecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): F[OAuth2SecurityScheme[L]]
  def getClassName(operation: Tracker[Operation], vendorPrefixes: List[String]): F[List[String]]
  def getParameterName(parameter: Tracker[Parameter]): F[String]
  def getBodyParameterSchema(parameter: Tracker[Parameter]): F[Tracker[Schema[_]]]
  def getHeaderParameterType(parameter: Tracker[Parameter]): F[Tracker[String]]
  def getPathParameterType(parameter: Tracker[Parameter]): F[Tracker[String]]
  def getQueryParameterType(parameter: Tracker[Parameter]): F[Tracker[String]]
  def getCookieParameterType(parameter: Tracker[Parameter]): F[Tracker[String]]
  def getFormParameterType(parameter: Tracker[Parameter]): F[Tracker[String]]
  def getRefParameterRef(parameter: Tracker[Parameter]): F[Tracker[String]]
  def getSerializableParameterType(parameter: Tracker[Parameter]): F[Tracker[String]]
  def fallbackParameterHandler(parameter: Tracker[Parameter]): F[SwaggerUtil.ResolvedType[L]]
  def getOperationId(operation: Tracker[Operation]): F[String]
  def getResponses(operationId: String, operation: Tracker[Operation]): F[NonEmptyList[(String, Tracker[ApiResponse])]]
  def getSimpleRef(ref: Tracker[Option[Schema[_]]]): F[String]
  def getItems(arr: Tracker[ArraySchema]): F[Tracker[Schema[_]]]
  def getType(model: Tracker[Schema[_]]): F[Tracker[String]]
  def fallbackPropertyTypeHandler(prop: Tracker[Schema[_]]): F[L#Type]
  def resolveType(name: String, protocolElems: List[StrictProtocolElems[L]]): F[StrictProtocolElems[L]]
  def fallbackResolveElems(lazyElems: List[LazyProtocolElems[L]]): F[List[StrictProtocolElems[L]]]
  def log: SwaggerLogAdapter[F]
}
