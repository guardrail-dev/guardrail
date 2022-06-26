package dev.guardrail.generators.scala

sealed abstract class CirceRefinedModelGenerator(val value: String, val toCirce: CirceModelGenerator) extends ModelGeneratorType
object CirceRefinedModelGenerator {
  case object V011 extends CirceRefinedModelGenerator("circe-refined-v0.11", CirceModelGenerator.V011)
  case object V012 extends CirceRefinedModelGenerator("circe-refined-v0.12", CirceModelGenerator.V012)

  val mapping: Map[String, CirceRefinedModelGenerator] = Map(
    "circe-refined" -> V012,
    V011.value      -> V011,
    V012.value      -> V012
  )
}
