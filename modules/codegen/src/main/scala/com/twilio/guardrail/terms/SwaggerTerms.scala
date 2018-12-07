package com.twilio.guardrail
package terms

import _root_.io.swagger.models.{ ArrayModel, Model, ModelImpl, Operation, Path, RefModel, Response }
import _root_.io.swagger.models.parameters.{
  BodyParameter,
  CookieParameter,
  FormParameter,
  HeaderParameter,
  Parameter,
  PathParameter,
  QueryParameter,
  RefParameter,
  SerializableParameter
}
import _root_.io.swagger.models.properties.{ ArrayProperty, Property, RefProperty }
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.languages.LA

class SwaggerTerms[L <: LA, F[_]](implicit I: InjectK[SwaggerTerm[L, ?], F]) {
  def extractOperations(paths: List[(String, Path)]): Free[F, List[RouteMeta]] =
    Free.inject[SwaggerTerm[L, ?], F](ExtractOperations(paths))
  def getClassName(operation: Operation): Free[F, List[String]] =
    Free.inject[SwaggerTerm[L, ?], F](GetClassName(operation))
  def getParameterName(parameter: Parameter): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetParameterName(parameter))
  def getBodyParameterSchema(parameter: BodyParameter): Free[F, Model] =
    Free.inject[SwaggerTerm[L, ?], F](GetBodyParameterSchema(parameter))

  def getHeaderParameterType(parameter: HeaderParameter): Free[F, String] = Free.inject[SwaggerTerm[L, ?], F](GetHeaderParameterType(parameter))
  def getPathParameterType(parameter: PathParameter): Free[F, String]     = Free.inject[SwaggerTerm[L, ?], F](GetPathParameterType(parameter))
  def getQueryParameterType(parameter: QueryParameter): Free[F, String]   = Free.inject[SwaggerTerm[L, ?], F](GetQueryParameterType(parameter))
  def getCookieParameterType(parameter: CookieParameter): Free[F, String] = Free.inject[SwaggerTerm[L, ?], F](GetCookieParameterType(parameter))
  def getFormParameterType(parameter: FormParameter): Free[F, String]     = Free.inject[SwaggerTerm[L, ?], F](GetFormParameterType(parameter))
  def getRefParameterRef(parameter: RefParameter): Free[F, String]        = Free.inject[SwaggerTerm[L, ?], F](GetRefParameterRef(parameter))

  def getSerializableParameterType(parameter: SerializableParameter): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetSerializableParameterType(parameter))
  def fallbackParameterHandler(parameter: Parameter): Free[F, SwaggerUtil.ResolvedType[L]] =
    Free.inject[SwaggerTerm[L, ?], F](FallbackParameterHandler(parameter))

  def getOperationId(operation: Operation): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetOperationId(operation))
  def getResponses(operationId: String, operation: Operation): Free[F, Map[String, Response]] =
    Free.inject[SwaggerTerm[L, ?], F](GetResponses(operationId, operation))
  def getSimpleRef(ref: RefModel): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetSimpleRef(ref))
  def getSimpleRefP(ref: RefProperty): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetSimpleRefP(ref))
  def getItems(arr: ArrayModel): Free[F, Property] =
    Free.inject[SwaggerTerm[L, ?], F](GetItems(arr))
  def getItemsP(arr: ArrayProperty): Free[F, Property] =
    Free.inject[SwaggerTerm[L, ?], F](GetItemsP(arr))
  def getType(model: ModelImpl): Free[F, String] =
    Free.inject[SwaggerTerm[L, ?], F](GetType(model))
  def fallbackPropertyTypeHandler(prop: Property): Free[F, L#Type] =
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
