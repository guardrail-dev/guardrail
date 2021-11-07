package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object guardrail {
  val project = baseModule("guardrail", "guardrail", file("modules/codegen"))
    .settings(
      guardrailJavaAsyncHttpVersion := "dev.guardrail" %% "guardrail-java-dropwizard" % "0.66.0",
      guardrailJavaDropwizardVersion := "dev.guardrail" %% "guardrail-java-dropwizard" % "0.66.0",
      guardrailJavaSpringMvcVersion := "dev.guardrail" %% "guardrail-java-dropwizard" % "0.66.0",
      guardrailJavaSupportVersion := "dev.guardrail" %% "guardrail-java-dropwizard" % "0.66.0",

      guardrailScalaAkkaHttpVersion := "dev.guardrail" %% "guardrail-scala-akka-http" % "0.68.0",
      guardrailScalaDropwizardVersion := "dev.guardrail" %% "guardrail-scala-dropwizard" % "0.66.0",
      guardrailScalaEndpointsVersion := "dev.guardrail" %% "guardrail-scala-endpoints" % "0.66.0",
      guardrailScalaHttp4sVersion := "dev.guardrail" %% "guardrail-scala-http4s" % "0.66.0",
      guardrailScalaSupportVersion := "dev.guardrail" %% "guardrail-scala-support" % "0.67.0"
    )
}
