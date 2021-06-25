package com.twilio.guardrail.generators.Scala.model

import scala.meta._

sealed trait ModelGeneratorType

sealed trait CirceModelGenerator extends ModelGeneratorType {
  def encoderObject: Type
  def encoderObjectCompanion: Term
  def print: Term.Name
}
object CirceModelGenerator {
  case object V011 extends CirceModelGenerator {
    def encoderObject: Type.Name          = t"ObjectEncoder"
    def encoderObjectCompanion: Term.Name = q"ObjectEncoder"
    def print: Term.Name                  = q"pretty"
  }
  case object V012 extends CirceModelGenerator {
    def encoderObject: Type.Select          = t"_root_.io.circe.Encoder.AsObject"
    def encoderObjectCompanion: Term.Select = q"_root_.io.circe.Encoder.AsObject"
    def print: Term.Name                    = q"print"
  }
}

case object JacksonModelGenerator extends ModelGeneratorType
