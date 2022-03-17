package dev.guardrail

sealed trait AuthImplementation
object AuthImplementation {
  case object Disable extends AuthImplementation
  case object Native  extends AuthImplementation
  case object Simple  extends AuthImplementation
  case object Custom  extends AuthImplementation
}
