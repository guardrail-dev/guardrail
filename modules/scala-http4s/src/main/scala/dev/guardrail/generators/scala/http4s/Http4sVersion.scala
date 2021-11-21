package dev.guardrail.generators.scala.http4s

sealed trait Http4sVersion
object Http4sVersion {
  case object V0_22 extends Http4sVersion
  case object V0_23 extends Http4sVersion
}
