package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.ModelImpl
import com.twilio.guardrail.languages.LA

sealed trait EnumProtocolTerm[L <: LA, T]
case class ExtractEnum[L <: LA](swagger: ModelImpl)                                                 extends EnumProtocolTerm[L, Either[String, List[String]]]
case class ExtractType[L <: LA](swagger: ModelImpl)                                                 extends EnumProtocolTerm[L, Either[String, L#Type]]
case class RenderMembers[L <: LA](clsName: String, elems: List[(String, L#TermName, L#TermSelect)]) extends EnumProtocolTerm[L, L#ObjectDefinition]
case class EncodeEnum[L <: LA](clsName: String)                                                     extends EnumProtocolTerm[L, L#ValueDefinition]
case class DecodeEnum[L <: LA](clsName: String)                                                     extends EnumProtocolTerm[L, L#ValueDefinition]
case class RenderClass[L <: LA](clsName: String, tpe: L#Type)                                       extends EnumProtocolTerm[L, L#ClassDefinition]
case class RenderCompanion[L <: LA](clsName: String,
                                    members: L#ObjectDefinition,
                                    accessors: List[L#ValueDefinition],
                                    values: L#ValueDefinition,
                                    encoder: L#ValueDefinition,
                                    decoder: L#ValueDefinition)
    extends EnumProtocolTerm[L, L#ObjectDefinition]
