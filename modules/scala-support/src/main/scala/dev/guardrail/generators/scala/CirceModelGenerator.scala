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

  def unapply(version: String): Option[CirceModelGenerator] = version match {
    case "circe"    => Some(V012)
    case V011.value => Some(V011)
    case V012.value => Some(V012)
    case _          => None
  }
}
