package dev.guardrail.generators.java.jackson

sealed abstract class JacksonVersion(val value: String)
case object JacksonVersion extends JacksonVersion("jackson") {
  val mapping: Map[String, JacksonVersion] = Map(
    JacksonVersion.value -> JacksonVersion
  )
}
