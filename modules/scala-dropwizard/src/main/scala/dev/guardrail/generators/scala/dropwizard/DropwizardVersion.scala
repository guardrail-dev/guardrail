package dev.guardrail.generators.scala.dropwizard

sealed abstract class DropwizardVersion(val value: String)
object DropwizardVersion extends DropwizardVersion("dropwizard") {
  val mapping: Map[String, DropwizardVersion] = Map(
    "dropwizard" -> DropwizardVersion
  )
}
