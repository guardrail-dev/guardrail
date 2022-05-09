package dev.guardrail.generators.java.jackson

sealed abstract class JacksonVersion(val value: String)
case object JacksonVersion extends JacksonVersion("jackson") {
  def unapply(version: String): Option[JacksonVersion] = version match {
    case JacksonVersion.value => Some(JacksonVersion)
    case _                    => None
  }
}
