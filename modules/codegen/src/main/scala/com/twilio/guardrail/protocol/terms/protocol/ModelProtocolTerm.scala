package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import com.twilio.guardrail.SwaggerUtil.ResolvedType
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.{ ProtocolParameter, StaticDefns, SuperClass }

sealed trait ModelProtocolTerm[L <: LA, T]
case class ExtractProperties[L <: LA](swagger: Schema[_]) extends ModelProtocolTerm[L, List[(String, Schema[_])]]
case class TransformProperty[L <: LA](clsName: String,
                                      name: String,
                                      prop: Schema[_],
                                      meta: ResolvedType[L],
                                      needCamelSnakeConversion: Boolean,
                                      concreteTypes: List[PropMeta[L]],
                                      isRequired: Boolean)
    extends ModelProtocolTerm[L, ProtocolParameter[L]]
case class RenderDTOClass[L <: LA](clsName: String, terms: List[L#MethodParameter], parents: List[SuperClass[L]] = Nil)
    extends ModelProtocolTerm[L, L#ClassDefinition]
case class EncodeModel[L <: LA](clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil)
    extends ModelProtocolTerm[L, L#ValueDefinition]
case class DecodeModel[L <: LA](clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil)
    extends ModelProtocolTerm[L, L#ValueDefinition]
case class RenderDTOStaticDefns[L <: LA](clsName: String, deps: List[L#TermName], encoder: L#ValueDefinition, decoder: L#ValueDefinition)
    extends ModelProtocolTerm[L, StaticDefns[L]]
