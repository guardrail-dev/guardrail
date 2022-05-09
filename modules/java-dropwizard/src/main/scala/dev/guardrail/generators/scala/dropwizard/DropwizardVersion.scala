package dev.guardrail.generators.java.dropwizard

sealed abstract class DropwizardVersion(val value: String)
object DropwizardVersion extends DropwizardVersion("dropwizard") {
  def unapply(version: String): Option[DropwizardVersion] = version match {
    case "dropwizard" => Some(DropwizardVersion)
    case _            => None
  }
}
