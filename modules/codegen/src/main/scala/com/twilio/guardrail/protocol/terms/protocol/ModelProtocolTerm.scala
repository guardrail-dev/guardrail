package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import com.twilio.guardrail.SwaggerUtil.ResolvedType
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.{ ProtocolParameter, StaticDefns, SuperClass }

sealed trait PropertyRequirement
object PropertyRequirement {
  case object Required         extends PropertyRequirement
  case object RequiredNullable extends PropertyRequirement
  case object Optional         extends PropertyRequirement
  case object OptionalNullable extends PropertyRequirement
}
