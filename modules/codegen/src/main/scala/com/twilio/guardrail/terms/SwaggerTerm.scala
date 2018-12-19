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
import com.twilio.guardrail.generators.{ GeneratorSettings, ScalaParameter, ScalaParameters }
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import scala.collection.JavaConverters._
import com.twilio.guardrail.terms.framework.FrameworkTerms

case class RouteMeta(path: String, method: HttpMethod, operation: Operation) {
  private val parameters = {
    Option(operation.getParameters)
      .map(_.asScala.toList)
  }

  def getParametersF[L <: LA, F[_]](
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, ScalaParameters[L]] =
    parameters
      .fold(Free.pure[F, List[ScalaParameter[L]]](List.empty))(ScalaParameter.fromParametersF(protocolElems))
      .map(new ScalaParameters[L](_))

  def getParameters(protocolElems: List[StrictProtocolElems[ScalaLanguage]]): Target[ScalaParameters[ScalaLanguage]] =
    parameters
      .map(ScalaParameter.fromParameters(protocolElems))
      .getOrElse(Target.pure(List.empty[ScalaParameter[ScalaLanguage]]))
      .map(new ScalaParameters[ScalaLanguage](_))
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
