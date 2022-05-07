package dev.guardrail.generators.scala.akkaHttp

sealed abstract class AkkaHttpVersion(val value: String)
object AkkaHttpVersion {
  case object V10_1 extends AkkaHttpVersion("akka-http-v10.1")
  case object V10_2 extends AkkaHttpVersion("akka-http-v10.2")
  def unapply(version: String): Option[AkkaHttpVersion] = version match {
    case V10_1.value => Some(V10_1)
    case V10_2.value => Some(V10_2)
    case _           => None
  }
}
