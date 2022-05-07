package dev.guardrail.generators.scala

import scala.meta._

sealed trait ModelGeneratorType {
  def value: String
}

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
    case V011.value => Some(V011)
    case V012.value => Some(V012)
    case _          => None
  }
}

sealed abstract class JacksonModelGenerator(val value: String) extends ModelGeneratorType
case object JacksonModelGenerator extends JacksonModelGenerator("jackson") {
  def unapply(version: String): Option[JacksonModelGenerator] = version match {
    case JacksonModelGenerator.value => Some(JacksonModelGenerator)
    case _                           => None
  }
}

sealed abstract class AkkaHttpVersion(val value: String)
object AkkaHttpVersion {
  case object V10_1 extends AkkaHttpVersion("akka-http-v10.1")
  case object V10_2 extends AkkaHttpVersion("akka-http-v10.2")
  def unapply(version: String): Option[AkkaHttpVersion] = version match {
    case V10_1.value => Some(V10_1)
    case V10_2.value => Some(V10_2)
    case _           => None
  }
}
