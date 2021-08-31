package dev.guardrail

import dev.guardrail.protocol.terms.protocol.PropertyRequirement

case class Context(
    framework: Option[String],
    customExtraction: Boolean,
    tracing: Boolean,
    modules: List[String],
    propertyRequirement: PropertyRequirement.Configured
)

object Context {
  val empty: Context = Context(
    None,
    customExtraction = false,
    tracing = false,
    modules = List.empty,
    propertyRequirement = PropertyRequirement.Configured(PropertyRequirement.OptionalLegacy, PropertyRequirement.OptionalLegacy)
  )
}
