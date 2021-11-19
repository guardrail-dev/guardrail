package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._
import wartremover.WartRemover.autoImport._

object scalaAkkaHttp {
  val akkaHttpVersion        = "10.2.7"
  val akkaVersion            = "2.6.17"
  val catsVersion            = "2.6.1"
  val circeVersion           = "0.14.1"
  val hibernateVersion       = "6.2.0.Final"
  val jacksonVersion         = "2.13.0"
  val javaxAnnotationVersion = "1.3.2"
  val javaxElVersion         = "3.0.0"
  val jaxbApiVersion         = "2.3.1"
  val scalatestVersion       = "3.2.10"

  val dependencies = Seq(
    "javax.annotation"  %  "javax.annotation-api" % javaxAnnotationVersion, // for jdk11
    "javax.xml.bind"    %  "jaxb-api"             % jaxbApiVersion, // for jdk11
  ) ++ Seq(
    "com.typesafe.akka" %% "akka-http"            % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-testkit"    % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-stream"          % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit"         % akkaVersion,
    "io.circe"          %% "circe-core"           % circeVersion,
    "io.circe"          %% "circe-jawn"           % circeVersion,
    "io.circe"          %% "circe-parser"         % circeVersion,
    "org.scalatest"     %% "scalatest"            % scalatestVersion % Test,
    "org.typelevel"     %% "cats-core"            % catsVersion
  ).map(_.cross(CrossVersion.for3Use2_13))

  val dependenciesJackson = Seq(
    "javax.annotation"               %  "javax.annotation-api"    % javaxAnnotationVersion, // for jdk11
    "javax.xml.bind"                 %  "jaxb-api"                % jaxbApiVersion, // for jdk11
    "com.fasterxml.jackson.core"     %  "jackson-core"            % jacksonVersion,
    "com.fasterxml.jackson.core"     %  "jackson-databind"        % jacksonVersion,
    "com.fasterxml.jackson.core"     %  "jackson-annotations"     % jacksonVersion,
    "com.fasterxml.jackson.datatype" %  "jackson-datatype-jsr310" % jacksonVersion,
    "org.hibernate"                  %  "hibernate-validator"     % hibernateVersion,
    "org.glassfish"                  %  "javax.el"                % javaxElVersion,
  ) ++ Seq(
    "com.typesafe.akka"              %% "akka-http"               % akkaHttpVersion,
    "com.typesafe.akka"              %% "akka-http-testkit"       % akkaHttpVersion,
    "com.typesafe.akka"              %% "akka-stream"             % akkaVersion,
    "com.typesafe.akka"              %% "akka-testkit"            % akkaVersion,
    "com.fasterxml.jackson.module"   %% "jackson-module-scala"    % jacksonVersion,
    "org.typelevel"                  %% "cats-core"               % catsVersion,
    "org.scalatest"                  %% "scalatest"               % scalatestVersion % Test,
  ).map(_.cross(CrossVersion.for3Use2_13))

  val project = commonModule("scala-akka-http")

  val sample =
    buildSampleProject("akkaHttp", dependencies)
      .settings(Compile / compile / wartremoverWarnings --= Seq(Wart.NonUnitStatements, Wart.Throw))
  val sampleJackson =
    buildSampleProject("akkaHttpJackson", dependenciesJackson)
      .settings(Compile / compile / wartremoverWarnings --= Seq(Wart.AsInstanceOf, Wart.NonUnitStatements, Wart.Null, Wart.OptionPartial, Wart.Throw))
}
