package dev.guardrail.generators.scala.http4s

sealed abstract class Http4sVersion(val value: String)
object Http4sVersion {
  case object V0_22 extends Http4sVersion("http4s-v0.22")
  case object V0_23 extends Http4sVersion("http4s-v0.23")

  val mapping: Map[String, Http4sVersion] = Map(
    "http4s"    -> V0_23,
    V0_22.value -> V0_22,
    V0_23.value -> V0_23
  )
}
