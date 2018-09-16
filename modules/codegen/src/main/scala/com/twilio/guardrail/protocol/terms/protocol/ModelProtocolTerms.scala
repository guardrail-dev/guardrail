package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.{ ComposedModel, ModelImpl }
import _root_.io.swagger.models.properties.Property
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.ProtocolParameter

import scala.meta._

class ModelProtocolTerms[F[_]](implicit I: InjectK[ModelProtocolTerm, F]) {

  def extractProperties(swagger: ModelImpl): Free[F, Either[String, List[(String, Property)]]] =
    Free.inject[ModelProtocolTerm, F](ExtractProperties(swagger))

  // For ADTs
  def extractChildProperties(
      parent: ModelImpl,
      child: ComposedModel,
      discriminator: String
  ): Free[F, Either[String, List[(String, Property)]]] =
    Free.inject[ModelProtocolTerm, F](ExtractChildProperties(parent, child, discriminator))

  def transformProperty(clsName: String, needCamelSnakeConversion: Boolean, concreteTypes: List[PropMeta])(
      name: String,
      prop: Property
  ): Free[F, ProtocolParameter] =
    Free.inject[ModelProtocolTerm, F](TransformProperty(clsName, name, prop, needCamelSnakeConversion, concreteTypes))

  def renderDTOClass(clsName: String, terms: List[Term.Param], parentName: Option[String]): Free[F, Defn.Class] =
    Free.inject[ModelProtocolTerm, F](RenderDTOClass(clsName, terms, parentName))

  def encodeModel(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]): Free[F, Stat] =
    Free.inject[ModelProtocolTerm, F](EncodeModel(clsName, needCamelSnakeConversion, params))

  def decodeModel(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]): Free[F, Stat] =
    Free.inject[ModelProtocolTerm, F](DecodeModel(clsName, needCamelSnakeConversion, params))

  def renderDTOCompanion(clsName: String, deps: List[Term.Name], encoder: Stat, decoder: Stat): Free[F, Defn.Object] =
    Free.inject[ModelProtocolTerm, F](RenderDTOCompanion(clsName, deps, encoder, decoder))
}

object ModelProtocolTerms {
  implicit def modelProtocolTerm[F[_]](implicit I: InjectK[ModelProtocolTerm, F]): ModelProtocolTerms[F] =
    new ModelProtocolTerms[F]
}
