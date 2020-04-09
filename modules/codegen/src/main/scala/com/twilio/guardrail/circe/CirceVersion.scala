package com.twilio.guardrail.circe

import scala.meta._

sealed trait CirceVersion {
  def encoderObject: Type
  def encoderObjectCompanion: Term
  def print: Term.Name
}
object CirceVersion {
  case object V011 extends CirceVersion {
    def encoderObject: Type.Name          = t"ObjectEncoder"
    def encoderObjectCompanion: Term.Name = q"ObjectEncoder"
    def print: Term.Name                  = q"pretty"
  }
  case object V012 extends CirceVersion {
    def encoderObject: Type.Select          = t"Encoder.AsObject"
    def encoderObjectCompanion: Term.Select = q"Encoder.AsObject"
    def print: Term.Name                    = q"print"
  }
}
