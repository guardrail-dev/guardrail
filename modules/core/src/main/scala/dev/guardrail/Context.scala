package dev.guardrail

import dev.guardrail.terms.protocol.PropertyRequirement

case class Context(
    framework: Option[String],
    customExtraction: Boolean,
    tracing: Boolean,
    modules: List[String],
    propertyRequirement: PropertyRequirement.Configured,
    tagsBehaviour: Context.TagsBehaviour
)

object Context {
  sealed trait TagsBehaviour
  case object TagsArePackages extends TagsBehaviour
  case object TagsAreIgnored  extends TagsBehaviour

  val empty: Context = Context(
    None,
    customExtraction = false,
    tracing = false,
    modules = List.empty,
    propertyRequirement = PropertyRequirement.Configured(PropertyRequirement.OptionalLegacy, PropertyRequirement.OptionalLegacy),
    tagsBehaviour = TagsAreIgnored
  )
}
