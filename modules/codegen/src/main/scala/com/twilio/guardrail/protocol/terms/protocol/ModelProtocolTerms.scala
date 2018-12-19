package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.Model
import _root_.io.swagger.models.properties.Property
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.{ ProtocolParameter, SuperClass }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.SwaggerUtil.ResolvedType

class ModelProtocolTerms[L <: LA, F[_]](implicit I: InjectK[ModelProtocolTerm[L, ?], F]) {
  def extractProperties(swagger: Model): Free[F, List[(String, Property)]] =
    Free.inject[ModelProtocolTerm[L, ?], F](ExtractProperties[L](swagger))
  def transformProperty(clsName: String, needCamelSnakeConversion: Boolean, concreteTypes: List[PropMeta[L]])(
      name: String,
      prop: Property,
      meta: ResolvedType[L]
  ): Free[F, ProtocolParameter[L]] =
    Free.inject[ModelProtocolTerm[L, ?], F](TransformProperty[L](clsName, name, prop, meta, needCamelSnakeConversion, concreteTypes))
  def renderDTOClass(clsName: String, terms: List[L#MethodParameter], parents: List[SuperClass[L]] = Nil): Free[F, L#ClassDefinition] =
    Free.inject[ModelProtocolTerm[L, ?], F](RenderDTOClass[L](clsName, terms, parents))
  def encodeModel(clsName: String,
                  needCamelSnakeConversion: Boolean,
                  params: List[ProtocolParameter[L]],
                  parents: List[SuperClass[L]] = Nil): Free[F, L#Statement] =
    Free.inject[ModelProtocolTerm[L, ?], F](EncodeModel[L](clsName, needCamelSnakeConversion, params, parents))
  def decodeModel(clsName: String,
                  needCamelSnakeConversion: Boolean,
                  params: List[ProtocolParameter[L]],
                  parents: List[SuperClass[L]] = Nil): Free[F, L#Statement] =
    Free.inject[ModelProtocolTerm[L, ?], F](DecodeModel[L](clsName, needCamelSnakeConversion, params, parents))
  def renderDTOCompanion(clsName: String, deps: List[L#TermName], encoder: L#Statement, decoder: L#Statement): Free[F, L#ObjectDefinition] =
    Free.inject[ModelProtocolTerm[L, ?], F](RenderDTOCompanion[L](clsName, deps, encoder, decoder))
}
object ModelProtocolTerms {
  implicit def modelProtocolTerm[L <: LA, F[_]](implicit I: InjectK[ModelProtocolTerm[L, ?], F]): ModelProtocolTerms[L, F] =
    new ModelProtocolTerms[L, F]
}
