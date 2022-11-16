package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object core {
  val catsVersion            = "2.8.0"

  val project = 
    commonModule("core")
      .settings(
        libraryDependencies ++= Seq(
          "io.swagger.parser.v3"        % "swagger-parser"                % "2.1.9",
        ) ++ Seq(
          "org.scala-lang.modules"      %% "scala-collection-compat"      % "2.8.1",
          "org.tpolecat"                %% "atto-core"                    % "0.9.5",
          "org.typelevel"               %% "cats-core"                    % catsVersion,
          "org.typelevel"               %% "cats-kernel"                  % catsVersion,
          "org.scala-lang.modules"      %% "scala-java8-compat"           % "1.0.2",
        ).map(_.cross(CrossVersion.for3Use2_13)),
      )
}
