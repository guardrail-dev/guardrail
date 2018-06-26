package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.ModelImpl
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._

sealed trait EnumProtocolTerm[T]
case class ExtractEnum(swagger: ModelImpl)                                        extends EnumProtocolTerm[Either[String, List[String]]]
case class ExtractType(swagger: ModelImpl, generatorSettings: GeneratorSettings)  extends EnumProtocolTerm[Either[String, Type]]
case class RenderMembers(clsName: String, elems: List[(String, Term.Name, Term)]) extends EnumProtocolTerm[Defn.Object]
case class EncodeEnum(clsName: String)                                            extends EnumProtocolTerm[Defn.Val]
case class DecodeEnum(clsName: String)                                            extends EnumProtocolTerm[Defn.Val]
case class RenderClass(clsName: String, tpe: Type)                                extends EnumProtocolTerm[Defn.Class]
case class RenderCompanion(clsName: String, members: Defn.Object, accessors: List[meta.Defn.Val], values: meta.Defn.Val, encoder: Defn.Val, decoder: Defn.Val)
    extends EnumProtocolTerm[Defn.Object]
