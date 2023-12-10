package dev.guardrail

sealed trait TagsBehaviour
object TagsBehaviour {
  case object PackageFromTags extends TagsBehaviour
  case object TagsAreIgnored  extends TagsBehaviour
}
