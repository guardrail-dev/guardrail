package com.twilio.guardrail.protocol.terms.protocol

import com.twilio.guardrail.SwaggerUtil
import com.twilio.guardrail.languages.LA

sealed trait ArrayProtocolTerm[L <: LA, T]
case class ExtractArrayType[L <: LA](arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]) extends ArrayProtocolTerm[L, L#Type]
