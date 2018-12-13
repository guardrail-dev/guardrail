package com.twilio.guardrail
package terms

import _root_.io.swagger.models.{ ArrayModel, ModelImpl, Operation, Path, RefModel, Response }
import _root_.io.swagger.models.parameters.Parameter
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
