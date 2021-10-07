package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object scalaHttp4s {
  val catsEffectVersion      = "2.5.4"
  val catsVersion            = "2.6.1"
  val circeVersion           = "0.14.1"
  val http4sVersion          = "0.23.5"
  val javaxAnnotationVersion = "1.3.2"
  val jaxbApiVersion         = "2.3.1"
  val scalatestVersion       = "3.2.10"

  val dependencies = Seq(
    "javax.annotation" %  "javax.annotation-api"  % javaxAnnotationVersion, // for jdk11
    "javax.xml.bind"   % "jaxb-api"               % jaxbApiVersion, // for jdk11
  ) ++ Seq(
    "io.circe"         %% "circe-core"            % circeVersion,
    "io.circe"         %% "circe-parser"          % circeVersion,
    "org.http4s"       %% "http4s-blaze-client"   % http4sVersion,
    "org.http4s"       %% "http4s-blaze-server"   % http4sVersion,
    "org.http4s"       %% "http4s-circe"          % http4sVersion,
    "org.http4s"       %% "http4s-dsl"            % http4sVersion,
    "org.scalatest"    %% "scalatest"             % scalatestVersion % Test,
    "org.typelevel"    %% "cats-core"             % catsVersion,
    "org.typelevel"    %% "cats-effect"           % catsEffectVersion
  ).map(_.cross(CrossVersion.for3Use2_13))

  val project = commonModule("scala-http4s")

  val sample = buildSampleProject("http4s", dependencies)
}
