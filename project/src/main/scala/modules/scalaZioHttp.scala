package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object scalaZioHttp {
  val catsVersion            = "2.7.0"
  val circeVersion           = "0.14.1"
  val javaxAnnotationVersion = "1.3.2"
  val jaxbApiVersion         = "2.3.1"
  val scalatestVersion       = "3.2.11"

  val dependencies = {
    val catsEffectVersion      = "3.3.5"
    val http4sVersion          = "0.23.9"

    val zioHttpVersion = "3.0.0-RC3"

    Seq(
      "javax.annotation" %  "javax.annotation-api"  % javaxAnnotationVersion, // for jdk11
      "javax.xml.bind"   % "jaxb-api"               % jaxbApiVersion, // for jdk11
      "dev.zio"          %% "zio-http"              % zioHttpVersion,
      "org.scalatest"    %% "scalatest"             % scalatestVersion % Test,
    ) ++ Seq(
      "io.circe"         %% "circe-core"            % circeVersion,
      "io.circe"         %% "circe-parser"          % circeVersion,
      "org.http4s"       %% "http4s-blaze-client"   % http4sVersion,
      "org.http4s"       %% "http4s-blaze-server"   % http4sVersion,
      "org.http4s"       %% "http4s-circe"          % http4sVersion,
      "org.http4s"       %% "http4s-dsl"            % http4sVersion,
      "org.typelevel"    %% "cats-core"             % catsVersion,
      "org.typelevel"    %% "cats-effect"           % catsEffectVersion,
    ).map(_.cross(CrossVersion.for3Use2_13))
  }

  val project = commonModule("scala-zio-http")

  val sample = buildSampleProject("zioHttp", dependencies)
}
