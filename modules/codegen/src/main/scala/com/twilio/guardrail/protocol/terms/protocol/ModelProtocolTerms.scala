package com.twilio.guardrail.protocol.terms.protocol

import io.swagger.v3.oas.models.media.Schema
import cats.{ InjectK, Monad }
import cats.arrow.FunctionK
import cats.free.Free
import com.twilio.guardrail.{ ProtocolParameter, StaticDefns, SuperClass }
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.SwaggerUtil.ResolvedType

abstract class ModelProtocolTerms[L <: LA, F[_]] extends FunctionK[ModelProtocolTerm[L, ?], F] {
  def MonadF: Monad[F]
  def extractProperties(swagger: Tracker[Schema[_]]): F[List[(String, Tracker[Schema[_]])]]
  def transformProperty(
      clsName: String,
      needCamelSnakeConversion: Boolean,
      concreteTypes: List[PropMeta[L]]
  )(
      name: String,
      prop: Schema[_],
      meta: ResolvedType[L],
      requirement: PropertyRequirement,
      isCustomType: Boolean,
      defaultValue: Option[L#Term]
  ): F[ProtocolParameter[L]]
  def renderDTOClass(clsName: String, terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil): F[L#ClassDefinition]
  def encodeModel(
      clsName: String,
      needCamelSnakeConversion: Boolean,
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): F[Option[L#ValueDefinition]]
  def decodeModel(
      clsName: String,
      needCamelSnakeConversion: Boolean,
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): F[Option[L#ValueDefinition]]
  def renderDTOStaticDefns(clsName: String, deps: List[L#TermName], encoder: Option[L#ValueDefinition], decoder: Option[L#ValueDefinition]): F[StaticDefns[L]]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newExtractProperties: Tracker[Schema[_]] => F[List[(String, Tracker[Schema[_]])]] = extractProperties _,
      newTransformProperty: (
          String,
          Boolean,
          List[PropMeta[L]]
      ) => (String, Schema[_], ResolvedType[L], PropertyRequirement, Boolean, Option[L#Term]) => F[ProtocolParameter[L]] = transformProperty _,
      newRenderDTOClass: (String, List[ProtocolParameter[L]], List[SuperClass[L]]) => F[L#ClassDefinition] = renderDTOClass _,
      newEncodeModel: (String, Boolean, List[ProtocolParameter[L]], List[SuperClass[L]]) => F[Option[L#ValueDefinition]] = encodeModel _,
      newDecodeModel: (String, Boolean, List[ProtocolParameter[L]], List[SuperClass[L]]) => F[Option[L#ValueDefinition]] = decodeModel _,
      newRenderDTOStaticDefns: (String, List[L#TermName], Option[L#ValueDefinition], Option[L#ValueDefinition]) => F[StaticDefns[L]] = renderDTOStaticDefns _
  ) = new ModelProtocolTerms[L, F] {
    def MonadF                                         = newMonadF
    def extractProperties(swagger: Tracker[Schema[_]]) = newExtractProperties(swagger)
    def transformProperty(
        clsName: String,
        needCamelSnakeConversion: Boolean,
        concreteTypes: List[PropMeta[L]]
    )(name: String, prop: Schema[_], meta: ResolvedType[L], requirement: PropertyRequirement, isCustomType: Boolean, defaultValue: Option[L#Term]) =
      newTransformProperty(clsName, needCamelSnakeConversion, concreteTypes)(name, prop, meta, requirement, isCustomType, defaultValue)
    def renderDTOClass(clsName: String, terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil) = newRenderDTOClass(clsName, terms, parents)
    def encodeModel(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil) =
      newEncodeModel(clsName, needCamelSnakeConversion, params, parents)
    def decodeModel(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil) =
      newDecodeModel(clsName, needCamelSnakeConversion, params, parents)
    def renderDTOStaticDefns(clsName: String, deps: List[L#TermName], encoder: Option[L#ValueDefinition], decoder: Option[L#ValueDefinition]) =
      newRenderDTOStaticDefns(clsName, deps, encoder, decoder)
  }

  def apply[T](term: ModelProtocolTerm[L, T]): F[T] = term match {
    case ExtractProperties(swagger) => extractProperties(swagger)
    case TransformProperty(clsName, name, property, meta, needCamelSnakeConversion, concreteTypes, requirement, isCustomType, defaultValue) =>
      transformProperty(clsName, needCamelSnakeConversion, concreteTypes)(name, property, meta, requirement, isCustomType, defaultValue)
    case RenderDTOClass(clsName, terms, parents)                         => renderDTOClass(clsName, terms, parents)
    case EncodeModel(clsName, needCamelSnakeConversion, params, parents) => encodeModel(clsName, needCamelSnakeConversion, params, parents)
    case DecodeModel(clsName, needCamelSnakeConversion, params, parents) => decodeModel(clsName, needCamelSnakeConversion, params, parents)
    case RenderDTOStaticDefns(clsName, deps, encoder, decoder)           => renderDTOStaticDefns(clsName, deps, encoder, decoder)
  }
}

object ModelProtocolTerms {
  implicit def modelProtocolTerm[L <: LA, F[_]](implicit I: InjectK[ModelProtocolTerm[L, ?], F]): ModelProtocolTerms[L, Free[F, ?]] =
    new ModelProtocolTerms[L, Free[F, ?]] {
      def MonadF = Free.catsFreeMonadForFree
      def extractProperties(swagger: Tracker[Schema[_]]): Free[F, List[(String, Tracker[Schema[_]])]] =
        Free.inject[ModelProtocolTerm[L, ?], F](ExtractProperties[L](swagger))
      def transformProperty(clsName: String, needCamelSnakeConversion: Boolean, concreteTypes: List[PropMeta[L]])(
          name: String,
          prop: Schema[_],
          meta: ResolvedType[L],
          requirement: PropertyRequirement,
          isCustomType: Boolean,
          defaultValue: Option[L#Term]
      ): Free[F, ProtocolParameter[L]] =
        Free.inject[ModelProtocolTerm[L, ?], F](
          TransformProperty[L](clsName, name, prop, meta, needCamelSnakeConversion, concreteTypes, requirement, isCustomType, defaultValue)
        )
      def renderDTOClass(clsName: String, terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil): Free[F, L#ClassDefinition] =
        Free.inject[ModelProtocolTerm[L, ?], F](RenderDTOClass[L](clsName, terms, parents))
      def encodeModel(
          clsName: String,
          needCamelSnakeConversion: Boolean,
          params: List[ProtocolParameter[L]],
          parents: List[SuperClass[L]] = Nil
      ): Free[F, Option[L#ValueDefinition]] = Free.inject[ModelProtocolTerm[L, ?], F](EncodeModel[L](clsName, needCamelSnakeConversion, params, parents))
      def decodeModel(
          clsName: String,
          needCamelSnakeConversion: Boolean,
          params: List[ProtocolParameter[L]],
          parents: List[SuperClass[L]] = Nil
      ): Free[F, Option[L#ValueDefinition]] = Free.inject[ModelProtocolTerm[L, ?], F](DecodeModel[L](clsName, needCamelSnakeConversion, params, parents))
      def renderDTOStaticDefns(
          clsName: String,
          deps: List[L#TermName],
          encoder: Option[L#ValueDefinition],
          decoder: Option[L#ValueDefinition]
      ): Free[F, StaticDefns[L]] = Free.inject[ModelProtocolTerm[L, ?], F](RenderDTOStaticDefns[L](clsName, deps, encoder, decoder))
    }
}
