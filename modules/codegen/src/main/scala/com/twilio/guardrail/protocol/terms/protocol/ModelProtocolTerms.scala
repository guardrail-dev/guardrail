package com.twilio.guardrail.protocol.terms.protocol

import io.swagger.v3.oas.models.media.Schema
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.{ ProtocolParameter, StaticDefns, SuperClass }
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.SwaggerUtil.ResolvedType

class ModelProtocolTerms[L <: LA, F[_]](implicit I: InjectK[ModelProtocolTerm[L, ?], F]) {
  def extractProperties(swagger: Tracker[Schema[_]]): Free[F, List[(String, Tracker[Schema[_]])]] =
    Free.inject[ModelProtocolTerm[L, ?], F](ExtractProperties[L](swagger))
  def transformProperty(clsName: String, needCamelSnakeConversion: Boolean, concreteTypes: List[PropMeta[L]])(
      name: String,
      prop: Schema[_],
      meta: ResolvedType[L],
      isRequired: Boolean,
      isCustomType: Boolean,
      defaultValue: Option[L#Term]
  ): Free[F, ProtocolParameter[L]] =
    Free.inject[ModelProtocolTerm[L, ?], F](
      TransformProperty[L](clsName, name, prop, meta, needCamelSnakeConversion, concreteTypes, isRequired, isCustomType, defaultValue)
    )
  def renderDTOClass(clsName: String, terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil): Free[F, L#ClassDefinition] =
    Free.inject[ModelProtocolTerm[L, ?], F](RenderDTOClass[L](clsName, terms, parents))
  def encodeModel(
      clsName: String,
      needCamelSnakeConversion: Boolean,
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): Free[F, Option[L#ValueDefinition]] =
    Free.inject[ModelProtocolTerm[L, ?], F](EncodeModel[L](clsName, needCamelSnakeConversion, params, parents))
  def decodeModel(
      clsName: String,
      needCamelSnakeConversion: Boolean,
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): Free[F, Option[L#ValueDefinition]] =
    Free.inject[ModelProtocolTerm[L, ?], F](DecodeModel[L](clsName, needCamelSnakeConversion, params, parents))
  def renderDTOStaticDefns(
      clsName: String,
      deps: List[L#TermName],
      encoder: Option[L#ValueDefinition],
      decoder: Option[L#ValueDefinition]
  ): Free[F, StaticDefns[L]] =
    Free.inject[ModelProtocolTerm[L, ?], F](RenderDTOStaticDefns[L](clsName, deps, encoder, decoder))
}
object ModelProtocolTerms {
  implicit def modelProtocolTerm[L <: LA, F[_]](implicit I: InjectK[ModelProtocolTerm[L, ?], F]): ModelProtocolTerms[L, F] =
    new ModelProtocolTerms[L, F]
}
