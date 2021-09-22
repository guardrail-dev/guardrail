package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object guardrail {
  val project = baseModule("guardrail", "guardrail", file("modules/codegen"))
}
