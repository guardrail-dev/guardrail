package com.twilio.guardrail
package terms

import _root_.io.swagger.models.{ ArrayModel, HttpMethod, Model, ModelImpl, Operation, Path, RefModel, Response }
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
import cats.MonadError
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.generators.{ ScalaParameter, ScalaParameters }
import com.twilio.guardrail.languages.LA
import scala.collection.JavaConverters._
import com.twilio.guardrail.terms.framework.FrameworkTerms

object RouteMeta {
  sealed abstract class ContentType(value: String)
  case object ApplicationJson   extends ContentType("application/json")
  case object MultipartFormData extends ContentType("multipart/form-data")
  case object TextPlain         extends ContentType("text/plain")
  object ContentType {
    def unapply(value: String): Option[ContentType] = value match {
      case "application/json"    => Some(ApplicationJson)
      case "multipart/form-data" => Some(MultipartFormData)
      case "text/plain"          => Some(TextPlain)
      case _                     => None
    }
  }
}
case class RouteMeta(path: String, method: HttpMethod, operation: Operation) {
  private val parameters = {
    Option(operation.getParameters)
      .map(_.asScala.toList)
  }

  def getParameters[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, ScalaParameters[L]] =
    parameters
      .fold(Free.pure[F, List[ScalaParameter[L]]](List.empty))(ScalaParameter.fromParameters(protocolElems))
      .map(new ScalaParameters[L](_))
}

sealed trait SwaggerTerm[L <: LA, T]
case class ExtractOperations[L <: LA](paths: List[(String, Path)])                         extends SwaggerTerm[L, List[RouteMeta]]
case class GetClassName[L <: LA](operation: Operation)                                     extends SwaggerTerm[L, List[String]]
case class GetParameterName[L <: LA](parameter: Parameter)                                 extends SwaggerTerm[L, String]
case class GetBodyParameterSchema[L <: LA](parameter: BodyParameter)                       extends SwaggerTerm[L, Model]
case class GetHeaderParameterType[L <: LA](parameter: HeaderParameter)                     extends SwaggerTerm[L, String]
case class GetPathParameterType[L <: LA](parameter: PathParameter)                         extends SwaggerTerm[L, String]
case class GetQueryParameterType[L <: LA](parameter: QueryParameter)                       extends SwaggerTerm[L, String]
case class GetCookieParameterType[L <: LA](parameter: CookieParameter)                     extends SwaggerTerm[L, String]
case class GetFormParameterType[L <: LA](parameter: FormParameter)                         extends SwaggerTerm[L, String]
case class GetSerializableParameterType[L <: LA](parameter: SerializableParameter)         extends SwaggerTerm[L, String]
case class GetRefParameterRef[L <: LA](parameter: RefParameter)                            extends SwaggerTerm[L, String]
case class FallbackParameterHandler[L <: LA](parameter: Parameter)                         extends SwaggerTerm[L, SwaggerUtil.ResolvedType[L]]
case class GetOperationId[L <: LA](operation: Operation)                                   extends SwaggerTerm[L, String]
case class GetResponses[L <: LA](operationId: String, operation: Operation)                extends SwaggerTerm[L, Map[String, Response]]
case class GetSimpleRef[L <: LA](ref: RefModel)                                            extends SwaggerTerm[L, String]
case class GetSimpleRefP[L <: LA](ref: RefProperty)                                        extends SwaggerTerm[L, String]
case class GetItems[L <: LA](arr: ArrayModel)                                              extends SwaggerTerm[L, Property]
case class GetItemsP[L <: LA](arr: ArrayProperty)                                          extends SwaggerTerm[L, Property]
case class GetType[L <: LA](model: ModelImpl)                                              extends SwaggerTerm[L, String]
case class FallbackPropertyTypeHandler[L <: LA](prop: Property)                            extends SwaggerTerm[L, L#Type]
case class ResolveType[L <: LA](name: String, protocolElems: List[StrictProtocolElems[L]]) extends SwaggerTerm[L, StrictProtocolElems[L]]
case class FallbackResolveElems[L <: LA](lazyElems: List[LazyProtocolElems[L]])            extends SwaggerTerm[L, List[StrictProtocolElems[L]]]
