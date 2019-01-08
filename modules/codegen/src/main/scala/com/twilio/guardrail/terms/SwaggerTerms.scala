package com.twilio.guardrail
package terms

import io.swagger.v3.oas.models._
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.languages.LA
import io.swagger.v3.oas.models.media.{ ArraySchema, ObjectSchema, Schema }
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.oas.models.responses.ApiResponse

class SwaggerTerms[L <: LA, F[_]](implicit I: InjectK[SwaggerTerm[L, ?], F]) {
  def extractOperations(paths: List[(String, PathItem)]): Free[F, List[RouteMeta]] =
    Free.inject[SwaggerTerm[L, ?], F](ExtractOperations(paths))
  def getClassName(operation: Operation): Free[F, List[String]] =
    Free.inject[SwaggerTerm[L, ?], F](GetClassName(operation))
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

  def getSimpleRefP(ref: Schema[_]): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetSimpleRefP(ref))

  def getItems(arr: ArraySchema): Free[F, Schema[_]] =
    Free.inject[SwaggerTerm[L, ?], F](GetItems(arr))

  def getItemsP(arr: ArraySchema): Free[F, Schema[_]] =
    Free.inject[SwaggerTerm[L, ?], F](GetItemsP(arr))

  def getType(model: Schema[_]): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetType(model))

  def fallbackPropertyTypeHandler(prop: Schema[_]): Free[F, L#Type] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackPropertyTypeHandler(prop))

  def resolveType(name: String, protocolElems: List[StrictProtocolElems[L]]): Free[F, StrictProtocolElems[L]] =
    Free.inject[SwaggerTerm[L, ?], F](ResolveType(name, protocolElems))
  def fallbackResolveElems(lazyElems: List[LazyProtocolElems[L]]): Free[F, List[StrictProtocolElems[L]]] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackResolveElems(lazyElems))
}
object SwaggerTerms {
  implicit def swaggerTerm[L <: LA, F[_]](implicit I: InjectK[SwaggerTerm[L, ?], F]): SwaggerTerms[L, F] =
    new SwaggerTerms[L, F]
}
