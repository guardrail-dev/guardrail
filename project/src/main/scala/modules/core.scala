package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object core {
  val catsVersion            = "2.6.1"

  val project = 
    commonModule("core")
      .settings(
        libraryDependencies ++= Seq(
          "com.github.javaparser"       % "javaparser-symbol-solver-core" % "3.22.1",
          "io.swagger.parser.v3"        % "swagger-parser"                % "2.0.28",
        ) ++ Seq(
          "org.scalameta"               %% "scalameta"                    % "4.4.29",
          "org.tpolecat"                %% "atto-core"                    % "0.9.5",
          "org.typelevel"               %% "cats-core"                    % catsVersion,
          "org.typelevel"               %% "cats-kernel"                  % catsVersion,
          "org.typelevel"               %% "cats-free"                    % catsVersion,
          "org.scala-lang.modules"      %% "scala-java8-compat"           % "1.0.2",
        ).map(_.cross(CrossVersion.for3Use2_13)),
      )
}
