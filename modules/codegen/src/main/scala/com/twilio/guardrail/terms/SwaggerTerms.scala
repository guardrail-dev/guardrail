package com.twilio.guardrail
package terms

import cats.InjectK
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

class SwaggerTerms[L <: LA, F[_]](implicit I: InjectK[SwaggerTerm[L, ?], F]) {
  def extractCommonRequestBodies(components: Option[Components]): Free[F, Map[String, RequestBody]] =
    Free.inject[SwaggerTerm[L, ?], F](ExtractCommonRequestBodies(components))

  def extractOperations(
      paths: Tracker[Mappish[List, String, PathItem]],
      commonRequestBodies: Map[String, RequestBody],
      globalSecurityRequirements: Option[SecurityRequirements]
  ): Free[F, List[RouteMeta]] =
    Free.inject[SwaggerTerm[L, ?], F](ExtractOperations(paths, commonRequestBodies, globalSecurityRequirements))

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
  def getParameterName(parameter: Parameter): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetParameterName(parameter))
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

  def getOperationId(operation: Tracker[Operation]): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetOperationId(operation))

  def getResponses(operationId: String, operation: Tracker[Operation]): Free[F, NonEmptyList[(String, Tracker[ApiResponse])]] =
    Free.inject[SwaggerTerm[L, ?], F](GetResponses(operationId, operation))

  def getSimpleRef(ref: Tracker[Option[Schema[_]]]): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetSimpleRef(ref))

  def getItems(arr: Tracker[ArraySchema]): Free[F, Tracker[Schema[_]]] =
    Free.inject[SwaggerTerm[L, ?], F](GetItems(arr))

  def getType(model: Tracker[Schema[_]]): Free[F, Tracker[String]] =
    Free.inject[SwaggerTerm[L, ?], F](GetType(model))

  def fallbackPropertyTypeHandler(prop: Tracker[Schema[_]]): Free[F, L#Type] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackPropertyTypeHandler(prop))

  def resolveType(name: String, protocolElems: List[StrictProtocolElems[L]]): Free[F, StrictProtocolElems[L]] =
    Free.inject[SwaggerTerm[L, ?], F](ResolveType(name, protocolElems))
  def fallbackResolveElems(lazyElems: List[LazyProtocolElems[L]]): Free[F, List[StrictProtocolElems[L]]] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackResolveElems(lazyElems))
  object log {
    def schemaToString(value: Schema[_]): String = "    " + value.toString().linesIterator.filterNot(_.contains(": null")).mkString("\n    ")
    def function[A](name: String): Free[F, A] => Free[F, A] = { func =>
      (push(name) *> func) <* pop
    }
    def push(name: String): Free[F, Unit] =
      Free.inject[SwaggerTerm[L, ?], F](LogPush[L](name))
    def pop: Free[F, Unit] =
      Free.inject[SwaggerTerm[L, ?], F](LogPop[L]())
    def debug(message: String): Free[F, Unit] =
      Free.inject[SwaggerTerm[L, ?], F](LogDebug[L](message))
    def info(message: String): Free[F, Unit] =
      Free.inject[SwaggerTerm[L, ?], F](LogInfo[L](message))
    def warning(message: String): Free[F, Unit] =
      Free.inject[SwaggerTerm[L, ?], F](LogWarning[L](message))
    def error(message: String): Free[F, Unit] =
      Free.inject[SwaggerTerm[L, ?], F](LogError[L](message))
  }
}
object SwaggerTerms {
  implicit def swaggerTerm[L <: LA, F[_]](implicit I: InjectK[SwaggerTerm[L, ?], F]): SwaggerTerms[L, F] =
    new SwaggerTerms[L, F]
}
