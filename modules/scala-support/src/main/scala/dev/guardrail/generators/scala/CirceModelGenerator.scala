package dev.guardrail.generators.scala

import scala.meta._

sealed abstract class CirceModelGenerator(val value: String) extends ModelGeneratorType {
  def encoderObject: Type
  def encoderObjectCompanion: Term
  def print: Term.Name
}

object CirceModelGenerator {
  case object V011 extends CirceModelGenerator("circe-v0.11") {
    def encoderObject: Type.Name          = t"ObjectEncoder"
    def encoderObjectCompanion: Term.Name = q"ObjectEncoder"
    def print: Term.Name                  = q"pretty"
  }
  case object V012 extends CirceModelGenerator("circe-v0.12") {
    def encoderObject: Type.Select          = t"_root_.io.circe.Encoder.AsObject"
    def encoderObjectCompanion: Term.Select = q"_root_.io.circe.Encoder.AsObject"
    def print: Term.Name                    = q"print"
  }

  val mapping: Map[String, CirceModelGenerator] = Map(
    "circe" -> V012,
    V011.value -> V011,
    V012.value -> V012
  )
}
