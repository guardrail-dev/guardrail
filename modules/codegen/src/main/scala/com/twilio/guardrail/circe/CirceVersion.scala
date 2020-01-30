package com.twilio.guardrail.circe

sealed trait CirceVersion {
  def encoderObject: String
  def print: String
}
object CirceVersion {
  case object V011 extends CirceVersion {
    def encoderObject = "ObjectEncoder"
    def print         = "pretty"
  }
  case object V012 extends CirceVersion {
    def encoderObject = "Encoder.AsObject"
    def print         = "print"
  }
}
