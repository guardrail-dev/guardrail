package com.twilio.guardrail
package terms

import cats.{ InjectK, Monad }
import cats.data.NonEmptyList
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.core.{ Mappish, Tracker }
import com.twilio.guardrail.languages.LA
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

  def extractCommonRequestBodies(components: Option[Components]): F[Map[String, RequestBody]]
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
  def getParameterName(parameter: Parameter): F[String]
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
object SwaggerTerms {
  implicit def swaggerTerm[L <: LA, F[_]](implicit I: InjectK[SwaggerTerm[L, ?], F]): SwaggerTerms[L, Free[F, ?]] = new SwaggerTerms[L, Free[F, ?]] {
    def MonadF = Free.catsFreeMonadForFree
    def extractCommonRequestBodies(components: Option[Components]): Free[F, Map[String, RequestBody]] =
      Free.inject[SwaggerTerm[L, ?], F](ExtractCommonRequestBodies(components))
    def extractOperations(
        paths: Tracker[Mappish[List, String, PathItem]],
        commonRequestBodies: Map[String, RequestBody],
        globalSecurityRequirements: Option[SecurityRequirements]
    ): Free[F, List[RouteMeta]] = Free.inject[SwaggerTerm[L, ?], F](ExtractOperations(paths, commonRequestBodies, globalSecurityRequirements))
    def extractApiKeySecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): Free[F, ApiKeySecurityScheme[L]] =
      Free.inject[SwaggerTerm[L, ?], F](ExtractApiKeySecurityScheme(schemeName, securityScheme, tpe))
    def extractHttpSecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): Free[F, HttpSecurityScheme[L]] =
      Free.inject[SwaggerTerm[L, ?], F](ExtractHttpSecurityScheme(schemeName, securityScheme, tpe))
    def extractOpenIdConnectSecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): Free[F, OpenIdConnectSecurityScheme[L]] =
      Free.inject[SwaggerTerm[L, ?], F](ExtractOpenIdConnectSecurityScheme(schemeName, securityScheme, tpe))
    def extractOAuth2SecurityScheme(schemeName: String, securityScheme: SwSecurityScheme, tpe: Option[L#Type]): Free[F, OAuth2SecurityScheme[L]] =
      Free.inject[SwaggerTerm[L, ?], F](ExtractOAuth2SecurityScheme(schemeName, securityScheme, tpe))
    def getClassName(operation: Tracker[Operation], vendorPrefixes: List[String]): Free[F, List[String]] =
      Free.inject[SwaggerTerm[L, ?], F](GetClassName(operation, vendorPrefixes))
    def getParameterName(parameter: Parameter): Free[F, String] = Free.inject[SwaggerTerm[L, ?], F](GetParameterName(parameter))
    def getBodyParameterSchema(parameter: Tracker[Parameter]): Free[F, Tracker[Schema[_]]] =
      Free.inject[SwaggerTerm[L, ?], F](GetBodyParameterSchema(parameter))
    def getHeaderParameterType(parameter: Tracker[Parameter]): Free[F, Tracker[String]] = Free.inject[SwaggerTerm[L, ?], F](GetHeaderParameterType(parameter))
    def getPathParameterType(parameter: Tracker[Parameter]): Free[F, Tracker[String]]   = Free.inject[SwaggerTerm[L, ?], F](GetPathParameterType(parameter))
    def getQueryParameterType(parameter: Tracker[Parameter]): Free[F, Tracker[String]]  = Free.inject[SwaggerTerm[L, ?], F](GetQueryParameterType(parameter))
    def getCookieParameterType(parameter: Tracker[Parameter]): Free[F, Tracker[String]] = Free.inject[SwaggerTerm[L, ?], F](GetCookieParameterType(parameter))
    def getFormParameterType(parameter: Tracker[Parameter]): Free[F, Tracker[String]]   = Free.inject[SwaggerTerm[L, ?], F](GetFormParameterType(parameter))
    def getRefParameterRef(parameter: Tracker[Parameter]): Free[F, Tracker[String]]     = Free.inject[SwaggerTerm[L, ?], F](GetRefParameterRef(parameter))
    def getSerializableParameterType(parameter: Tracker[Parameter]): Free[F, Tracker[String]] =
      Free.inject[SwaggerTerm[L, ?], F](GetSerializableParameterType(parameter))
    def fallbackParameterHandler(parameter: Tracker[Parameter]): Free[F, SwaggerUtil.ResolvedType[L]] =
      Free.inject[SwaggerTerm[L, ?], F](FallbackParameterHandler(parameter))
    def getOperationId(operation: Tracker[Operation]): Free[F, String] = Free.inject[SwaggerTerm[L, ?], F](GetOperationId(operation))
    def getResponses(operationId: String, operation: Tracker[Operation]): Free[F, NonEmptyList[(String, Tracker[ApiResponse])]] =
      Free.inject[SwaggerTerm[L, ?], F](GetResponses(operationId, operation))
    def getSimpleRef(ref: Tracker[Option[Schema[_]]]): Free[F, String]         = Free.inject[SwaggerTerm[L, ?], F](GetSimpleRef(ref))
    def getItems(arr: Tracker[ArraySchema]): Free[F, Tracker[Schema[_]]]       = Free.inject[SwaggerTerm[L, ?], F](GetItems(arr))
    def getType(model: Tracker[Schema[_]]): Free[F, Tracker[String]]           = Free.inject[SwaggerTerm[L, ?], F](GetType(model))
    def fallbackPropertyTypeHandler(prop: Tracker[Schema[_]]): Free[F, L#Type] = Free.inject[SwaggerTerm[L, ?], F](FallbackPropertyTypeHandler(prop))
    def resolveType(name: String, protocolElems: List[StrictProtocolElems[L]]): Free[F, StrictProtocolElems[L]] =
      Free.inject[SwaggerTerm[L, ?], F](ResolveType(name, protocolElems))
    def fallbackResolveElems(lazyElems: List[LazyProtocolElems[L]]): Free[F, List[StrictProtocolElems[L]]] =
      Free.inject[SwaggerTerm[L, ?], F](FallbackResolveElems(lazyElems))
    def log: SwaggerLogAdapter[Free[F, ?]] = new SwaggerLogAdapter[Free[F, ?]] {
      def function[A](name: String): Free[F, A] => Free[F, A] = { func =>
        push(name) *> func <* pop
      }
      def push(name: String): Free[F, Unit]       = Free.inject[SwaggerTerm[L, ?], F](LogPush[L](name))
      def pop: Free[F, Unit]                      = Free.inject[SwaggerTerm[L, ?], F](LogPop[L]())
      def debug(message: String): Free[F, Unit]   = Free.inject[SwaggerTerm[L, ?], F](LogDebug[L](message))
      def info(message: String): Free[F, Unit]    = Free.inject[SwaggerTerm[L, ?], F](LogInfo[L](message))
      def warning(message: String): Free[F, Unit] = Free.inject[SwaggerTerm[L, ?], F](LogWarning[L](message))
      def error(message: String): Free[F, Unit]   = Free.inject[SwaggerTerm[L, ?], F](LogError[L](message))
    }
  }
}
