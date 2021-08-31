package dev.guardrail

sealed trait CodegenTarget
object CodegenTarget {
  case object Client extends CodegenTarget
  case object Server extends CodegenTarget
  case object Models extends CodegenTarget
}
