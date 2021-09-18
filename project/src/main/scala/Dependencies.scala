package dev.guardrail.sbt

import sbt._
import sbt.Keys._

object Dependencies {
  val akkaVersion            = "2.6.16"
  val akkaHttpVersion        = "10.2.6"
  val catsVersion            = "2.6.1"
  val catsEffectVersion      = "2.5.4"
  val circeVersion           = "0.14.1"
  val http4sVersion          = "0.22.4"
  val scalatestVersion       = "3.2.10"
  val endpointsVersion       = "1.3.0"
  val endpointsCatsVersion   = "2.4.1"
  val endpointsCirceVersion  = "0.14.1"
  val ahcVersion             = "2.8.1"
  val dropwizardVersion      = "1.3.29"
  val dropwizardScalaVersion = "1.3.7-1"
  val jerseyVersion          = "2.25.1"
  val jaxbApiVersion         = "2.3.1"
  val javaxAnnotationVersion = "1.3.2"
  val springBootVersion      = "2.5.4"
  val jacksonVersion         = "2.12.5"
  val hibernateVersion       = "6.2.0.Final"
  val javaxElVersion         = "3.0.0"
  val vavrVersion            = "0.10.3"
  val dropwizardVavrVersion  = "1.3.0-4"

  // TAKE CARE WHEN UPDATING THESE
  val eclipseFormatterDependencies = Seq(
    "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.24.0",
    // These version pins are necessary because a bunch of transitive dependencies
    // are specified via an allowed version range rather than being pinned to a
    // particular version.  Unfortunately, at some point some of them started
    // being compiled targeting Java 11, which breaks builds for people who are
    // still building their projects with a JDK8 distribution.  Pinning only
    // the Java11-compiled dependencies is not enough, as some of them are not
    // mutually compatible.
    "org.eclipse.platform" % "org.eclipse.core.commands"       % "3.10.0",
    "org.eclipse.platform" % "org.eclipse.core.contenttype"    % "3.7.1000",
    "org.eclipse.platform" % "org.eclipse.core.expressions"    % "3.7.100",
    "org.eclipse.platform" % "org.eclipse.core.filesystem"     % "1.9.0",
    "org.eclipse.platform" % "org.eclipse.core.jobs"           % "3.11.0",
    "org.eclipse.platform" % "org.eclipse.core.resources"      % "3.14.0",
    "org.eclipse.platform" % "org.eclipse.core.runtime"        % "3.20.100",
    "org.eclipse.platform" % "org.eclipse.equinox.app"         % "1.5.100",
    "org.eclipse.platform" % "org.eclipse.equinox.common"      % "3.14.100",
    "org.eclipse.platform" % "org.eclipse.equinox.preferences" % "3.8.200",
    "org.eclipse.platform" % "org.eclipse.equinox.registry"    % "3.10.200",
    "org.eclipse.platform" % "org.eclipse.osgi"                % "3.16.300",
    "org.eclipse.platform" % "org.eclipse.text"                % "3.11.0",
  )

  val testDependencies = Seq(
    "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    "org.scalacheck" %% "scalacheck" % "1.15.4" % Test,
    "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
  ).map(_.cross(CrossVersion.for3Use2_13))


  val akkaProjectDependencies = Seq(
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

  val akkaJacksonProjectDependencies = Seq(
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

  val http4sProjectDependencies = Seq(
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

  val dropwizardProjectDependencies = Seq(
    "javax.annotation"           %  "javax.annotation-api"   % javaxAnnotationVersion, // for jdk11
    "javax.xml.bind"             %  "jaxb-api"               % jaxbApiVersion, // for jdk11
    "io.dropwizard"              %  "dropwizard-core"        % dropwizardVersion,
    "io.dropwizard"              %  "dropwizard-forms"       % dropwizardVersion,
    "org.asynchttpclient"        %  "async-http-client"      % ahcVersion,
    "junit"                      %  "junit"                  % "4.13.2"             % Test,
    "nl.jqno.equalsverifier"     %  "equalsverifier"         % "3.7.1"            % Test,
    "com.novocode"               %  "junit-interface"        % "0.11"             % Test,
    "com.github.tomakehurst"     %  "wiremock"               % "2.27.2"           % Test,
    "io.dropwizard"              %  "dropwizard-testing"     % dropwizardVersion  % Test,
    "org.glassfish.jersey.test-framework.providers" % "jersey-test-framework-provider-grizzly2" % jerseyVersion % Test
  ) ++ Seq(
    "org.mockito"                %% "mockito-scala"          % "1.16.42"           % Test,
    "org.scala-lang.modules"     %% "scala-java8-compat"     % "1.0.0"            % Test,
    "org.scalatest"              %% "scalatest"              % scalatestVersion   % Test,
  ).map(_.cross(CrossVersion.for3Use2_13))

  val dropwizardScalaProjectDependencies = Seq(
    "javax.annotation"               %  "javax.annotation-api"    % javaxAnnotationVersion, // for jdk11
    "javax.xml.bind"                 %  "jaxb-api"                % jaxbApiVersion, // for jdk11
    "io.dropwizard"                  %  "dropwizard-core"         % dropwizardVersion,
    "io.dropwizard"                  %  "dropwizard-forms"        % dropwizardVersion,
    "com.fasterxml.jackson.datatype" %  "jackson-datatype-jsr310" % jacksonVersion,
    "junit"                          %  "junit"                   % "4.13.2"             % Test,
    "com.novocode"                   %  "junit-interface"         % "0.11"             % Test,
    "com.github.tomakehurst"         %  "wiremock"                % "2.27.2"           % Test,
    "io.dropwizard"                  %  "dropwizard-testing"      % dropwizardVersion  % Test,
    "org.glassfish.jersey.test-framework.providers" % "jersey-test-framework-provider-grizzly2" % jerseyVersion % Test,
  ) ++ Seq(
    "com.datasift.dropwizard.scala"  %% "dropwizard-scala-core"   % dropwizardScalaVersion,
    "com.fasterxml.jackson.module"   %% "jackson-module-scala"    % jacksonVersion,
    "org.typelevel"                  %% "cats-core"               % catsVersion,
    "org.scala-lang.modules"         %% "scala-java8-compat"      % "1.0.0"            % Test,
    "org.scalatest"                  %% "scalatest"               % scalatestVersion   % Test,
    "org.mockito"                    %% "mockito-scala-scalatest" % "1.16.42"           % Test,
  ).map(_.cross(CrossVersion.for3Use2_13))

  val dropwizardVavrProjectDependencies = dropwizardProjectDependencies ++ Seq(
    "io.vavr"               % "vavr"            % vavrVersion,
    "io.vavr"               % "vavr-jackson"    % vavrVersion,
    "io.dropwizard.modules" % "dropwizard-vavr" % dropwizardVavrVersion,
  )

  val endpointsProjectDependencies = Seq(
    "io.circe"          %% "circe-core"          % endpointsCirceVersion,
    "io.circe"          %% "circe-parser"        % endpointsCirceVersion,
    "org.endpoints4s"   %% "algebra"             % endpointsVersion,
    "org.scalatest"     %% "scalatest"           % scalatestVersion % Test,
    "org.typelevel"     %% "cats-core"           % endpointsCatsVersion
  ).map(_.cross(CrossVersion.for3Use2_13))

  val springProjectDependencies = Seq(
    "org.springframework.boot"   %  "spring-boot-starter-web"  % springBootVersion,
    "javax.annotation"           %  "javax.annotation-api"    % javaxAnnotationVersion, // for jdk11
    "javax.validation"           %  "validation-api"           % "2.0.1.Final",
    "junit"                      %  "junit"                    % "4.13.2"           % Test,
    "org.springframework.boot"   %  "spring-boot-starter-test" % springBootVersion  % Test,
  ) ++ Seq(
    "org.scala-lang.modules"     %% "scala-java8-compat"       % "1.0.0"            % Test,
    "org.scalatest"              %% "scalatest"                % scalatestVersion   % Test,
    "org.mockito"                %% "mockito-scala"            % "1.16.42"           % Test,
  ).map(_.cross(CrossVersion.for3Use2_13))
}
