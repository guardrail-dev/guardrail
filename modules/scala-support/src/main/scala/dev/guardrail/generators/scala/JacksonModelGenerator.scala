package dev.guardrail.generators.scala

sealed abstract class JacksonModelGenerator(val value: String) extends ModelGeneratorType
case object JacksonModelGenerator extends JacksonModelGenerator("jackson") {
  def unapply(version: String): Option[JacksonModelGenerator] = version match {
    case JacksonModelGenerator.value => Some(JacksonModelGenerator)
    case _                           => None
  }
}
