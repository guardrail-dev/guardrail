package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object javaAsyncHttp {
  val project = commonModule("java-async-http")
    .settings(
      guardrailJavaSupportVersion := "dev.guardrail" %% "guardrail-java-support" % "0.66.0"
    )
}
