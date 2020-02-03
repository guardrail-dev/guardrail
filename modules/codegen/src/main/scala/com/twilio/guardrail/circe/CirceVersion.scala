package com.twilio.guardrail.circe

import scala.meta._

sealed trait CirceVersion {
  def encoderObject: Type
  def encoderObjectCompanion: Term
  def print: Term.Name
}
object CirceVersion {
  case object V011 extends CirceVersion {
    def encoderObject          = t"ObjectEncoder"
    def encoderObjectCompanion = q"ObjectEncoder"
    def print                  = q"pretty"
  }
  case object V012 extends CirceVersion {
    def encoderObject          = t"Encoder.AsObject"
    def encoderObjectCompanion = q"Encoder.AsObject"
    def print                  = q"print"
  }
}
