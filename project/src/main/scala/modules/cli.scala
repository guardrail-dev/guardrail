package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object cli {
  val project = commonModule("cli")
    .settings(
      guardrailVersion := "dev.guardrail" %% "guardrail" % "0.67.0"
    )
}
