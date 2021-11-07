package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object scalaEndpoints {
  val endpointsCatsVersion   = "2.4.1"
  val endpointsCirceVersion  = "0.14.1"
  val endpointsVersion       = "1.3.0"
  val scalatestVersion       = "3.2.10"

  val dependencies = Seq(
    "io.circe"          %% "circe-core"          % endpointsCirceVersion,
    "io.circe"          %% "circe-parser"        % endpointsCirceVersion,
    "org.endpoints4s"   %% "algebra"             % endpointsVersion,
    "org.scalatest"     %% "scalatest"           % scalatestVersion % Test,
    "org.typelevel"     %% "cats-core"           % endpointsCatsVersion
  ).map(_.cross(CrossVersion.for3Use2_13))


  val project = commonModule("scala-endpoints")
    .settings(
      guardrailScalaSupportVersion := "dev.guardrail" %% "guardrail-scala-support" % "0.67.0"
    )

  val sample = buildSampleProject("endpoints", dependencies)
}
