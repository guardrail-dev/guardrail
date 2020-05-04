package com.twilio.guardrail

import com.twilio.guardrail.protocol.terms.protocol.PropertyRequirement

case class Context(framework: Option[String], tracing: Boolean, modules: List[String], propertyRequirement: PropertyRequirement.Configured)

object Context {
  val empty: Context = Context(
    None,
    tracing = false,
    modules = List.empty,
    propertyRequirement = PropertyRequirement.Configured(PropertyRequirement.OptionalLegacy, PropertyRequirement.OptionalLegacy)
  )
}
