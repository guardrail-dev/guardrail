package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import com.twilio.guardrail.StaticDefns
import com.twilio.guardrail.languages.LA

sealed trait EnumProtocolTerm[L <: LA, T]
case class ExtractEnum[L <: LA](swagger: Schema[_])                                                            extends EnumProtocolTerm[L, Either[String, List[String]]]
case class RenderMembers[L <: LA](clsName: String, elems: List[(String, L#TermName, L#TermSelect)])            extends EnumProtocolTerm[L, Option[L#ObjectDefinition]]
case class EncodeEnum[L <: LA](clsName: String)                                                                extends EnumProtocolTerm[L, Option[L#ValueDefinition]]
case class DecodeEnum[L <: LA](clsName: String)                                                                extends EnumProtocolTerm[L, Option[L#ValueDefinition]]
case class RenderClass[L <: LA](clsName: String, tpe: L#Type, elems: List[(String, L#TermName, L#TermSelect)]) extends EnumProtocolTerm[L, L#ClassDefinition]
case class RenderStaticDefns[L <: LA](clsName: String,
                                      members: Option[L#ObjectDefinition],
                                      accessors: List[L#TermName],
                                      encoder: Option[L#ValueDefinition],
                                      decoder: Option[L#ValueDefinition])
    extends EnumProtocolTerm[L, StaticDefns[L]]
case class BuildAccessor[L <: LA](clsName: String, termName: String) extends EnumProtocolTerm[L, L#TermSelect]
