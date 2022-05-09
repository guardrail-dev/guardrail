package dev.guardrail.generators.scala.http4s

sealed abstract class Http4sVersion(val value: String)
object Http4sVersion {
  case object V0_22 extends Http4sVersion("http4s-v0.22")
  case object V0_23 extends Http4sVersion("http4s-v0.23")

  def unapply(version: String): Option[Http4sVersion] = version match {
    case "http4s"    => Some(V0_23)
    case V0_22.value => Some(V0_22)
    case V0_23.value => Some(V0_23)
    case _           => None
  }
}
