package dev.guardrail.generators.scala.akkaHttp

sealed abstract class AkkaHttpVersion(val value: String)
object AkkaHttpVersion {
  case object V10_1 extends AkkaHttpVersion("akka-http-v10.1")
  case object V10_2 extends AkkaHttpVersion("akka-http-v10.2")

  val mapping: Map[String, AkkaHttpVersion] = Map(
    "akka-http" -> V10_2,
    V10_1.value -> V10_1,
    V10_2.value -> V10_2
  )
}
