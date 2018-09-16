package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.{ ComposedModel, ModelImpl }
import _root_.io.swagger.models.properties.Property
import com.twilio.guardrail.ProtocolParameter
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._

sealed trait ModelProtocolTerm[T]
case class ExtractProperties(swagger: ModelImpl) extends ModelProtocolTerm[Either[String, List[(String, Property)]]]
case class ExtractChildProperties(parent: ModelImpl, child: ComposedModel, discriminator: String)
    extends ModelProtocolTerm[Either[String, List[(String, Property)]]]

case class TransformProperty(
    clsName: String,
    name: String,
    prop: Property,
    needCamelSnakeConversion: Boolean,
    concreteTypes: List[PropMeta]
) extends ModelProtocolTerm[ProtocolParameter]

case class RenderDTOClass(clsName: String, terms: List[Term.Param], parentName: Option[String])             extends ModelProtocolTerm[Defn.Class]
case class EncodeModel(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]) extends ModelProtocolTerm[Stat]
case class DecodeModel(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]) extends ModelProtocolTerm[Stat]
case class RenderDTOCompanion(clsName: String, deps: List[Term.Name], encoder: Stat, decoder: Stat)         extends ModelProtocolTerm[Defn.Object]
