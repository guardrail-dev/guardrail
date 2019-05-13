package com.twilio.guardrail
package terms

import cats.InjectK
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.languages.LA
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media.{ ArraySchema, Schema }
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.oas.models.responses.ApiResponse
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }

class SwaggerTerms[L <: LA, F[_]](implicit I: InjectK[SwaggerTerm[L, ?], F]) {
  def extractOperations(paths: List[(String, PathItem)]): Free[F, List[RouteMeta]] =
    Free.inject[SwaggerTerm[L, ?], F](ExtractOperations(paths))
  def extractSecuritySchemes(securitySchemes: Map[String, SwSecurityScheme], vendorPrefixes: List[String]): Free[F, Map[String, SecurityScheme]] =
    Free.inject[SwaggerTerm[L, ?], F](ExtractSecuritySchemes(securitySchemes, vendorPrefixes))
  def getClassName(operation: Operation, vendorPrefixes: List[String]): Free[F, List[String]] =
    Free.inject[SwaggerTerm[L, ?], F](GetClassName(operation, vendorPrefixes))
  def getParameterName(parameter: Parameter): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetParameterName(parameter))
  def getBodyParameterSchema(parameter: Parameter): Free[F, Schema[_]] =
    Free.inject[SwaggerTerm[L, ?], F](GetBodyParameterSchema(parameter))

  def getHeaderParameterType(parameter: Parameter): Free[F, String] = Free.inject[SwaggerTerm[L, ?], F](GetHeaderParameterType(parameter))
  def getPathParameterType(parameter: Parameter): Free[F, String]   = Free.inject[SwaggerTerm[L, ?], F](GetPathParameterType(parameter))
  def getQueryParameterType(parameter: Parameter): Free[F, String]  = Free.inject[SwaggerTerm[L, ?], F](GetQueryParameterType(parameter))
  def getCookieParameterType(parameter: Parameter): Free[F, String] = Free.inject[SwaggerTerm[L, ?], F](GetCookieParameterType(parameter))
  def getFormParameterType(parameter: Parameter): Free[F, String]   = Free.inject[SwaggerTerm[L, ?], F](GetFormParameterType(parameter))
  def getRefParameterRef(parameter: Parameter): Free[F, String]     = Free.inject[SwaggerTerm[L, ?], F](GetRefParameterRef(parameter))

  def getSerializableParameterType(parameter: Parameter): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetSerializableParameterType(parameter))
  def fallbackParameterHandler(parameter: Parameter): Free[F, SwaggerUtil.ResolvedType[L]] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackParameterHandler(parameter))

  def getOperationId(operation: Operation): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetOperationId(operation))

  def getResponses(operationId: String, operation: Operation): Free[F, Map[String, ApiResponse]] =
    Free.inject[SwaggerTerm[L, ?], F](GetResponses(operationId, operation))

  def getSimpleRef(ref: Schema[_]): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetSimpleRef(ref))

  def getItems(arr: ArraySchema): Free[F, Schema[_]] =
    Free.inject[SwaggerTerm[L, ?], F](GetItems(arr))

  def getType(model: Schema[_]): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetType(model))

  def fallbackPropertyTypeHandler(prop: Schema[_]): Free[F, L#Type] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackPropertyTypeHandler(prop))

  def resolveType(name: String, protocolElems: List[StrictProtocolElems[L]]): Free[F, StrictProtocolElems[L]] =
    Free.inject[SwaggerTerm[L, ?], F](ResolveType(name, protocolElems))
  def fallbackResolveElems(lazyElems: List[LazyProtocolElems[L]]): Free[F, List[StrictProtocolElems[L]]] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackResolveElems(lazyElems))
  object log {
    def schemaToString(value: Schema[_]): String = "    " + value.toString().lines.filterNot(_.contains(": null")).mkString("\n    ")
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
