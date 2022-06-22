package dev.guardrail.generators.scala

sealed abstract class JacksonModelGenerator(val value: String) extends ModelGeneratorType
case object JacksonModelGenerator extends JacksonModelGenerator("jackson") {
  val mapping: Map[String, JacksonModelGenerator] = Map(
    JacksonModelGenerator.value -> JacksonModelGenerator
  )
}
