package dev.guardrail.sbt.modules

import dev.guardrail.sbt.Build._

import sbt._
import sbt.Keys._

object javaDropwizard {
  val ahcVersion             = "2.12.3"
  val dropwizardVavrVersion  = "2.0.24-1"
  val dropwizardVersion      = "2.0.25"
  val javaxAnnotationVersion = "1.3.2"
  val jaxbApiVersion         = "2.3.1"
  val jerseyVersion          = "2.25.1"
  val scalatestVersion       = "3.2.12"
  val vavrVersion            = "0.10.3"
  val hibernateCoreVersion      = "5.5.7.Final"
  val hibernateValidatorVersion = "6.1.7.Final"

  val dependencies = Seq(
    "javax.annotation"           %  "javax.annotation-api"   % javaxAnnotationVersion, // for jdk11
    "javax.xml.bind"             %  "jaxb-api"               % jaxbApiVersion, // for jdk11
    "io.dropwizard"              %  "dropwizard-core"        % dropwizardVersion,
    "io.dropwizard"              %  "dropwizard-forms"       % dropwizardVersion,
    "org.asynchttpclient"        %  "async-http-client"      % ahcVersion,
    "org.hibernate"              %  "hibernate-core"         % hibernateCoreVersion,
    "org.hibernate.validator"    %  "hibernate-validator"    % hibernateValidatorVersion,
    "junit"                      %  "junit"                  % "4.13.2"           % Test,
    "nl.jqno.equalsverifier"     %  "equalsverifier"         % "3.10"             % Test,
    "com.github.sbt"             %  "junit-interface"        % "0.13.3"           % Test,
    "com.github.tomakehurst"     %  "wiremock"               % "2.27.2"           % Test,
    "io.dropwizard"              %  "dropwizard-testing"     % dropwizardVersion  % Test,
    "org.assertj"                %  "assertj-core"           % "3.23.1"           % Test,
    "org.glassfish.jersey.test-framework.providers" % "jersey-test-framework-provider-grizzly2" % jerseyVersion % Test,
  ) ++ Seq(
    "org.mockito"                %% "mockito-scala"          % "1.17.7"           % Test,
    "org.scala-lang.modules"     %% "scala-java8-compat"     % "1.0.2"            % Test,
    "org.scalatest"              %% "scalatest"              % scalatestVersion   % Test,
  ).map(_.cross(CrossVersion.for3Use2_13))

  val dependenciesVavr = dependencies ++ Seq(
    "io.vavr"               % "vavr"            % vavrVersion,
    "io.vavr"               % "vavr-jackson"    % vavrVersion,
    "io.dropwizard.modules" % "dropwizard-vavr" % dropwizardVavrVersion,
  )


  val project = commonModule("java-dropwizard")

  val sample = buildSampleProject("dropwizard", dependencies).settings(scalacOptions -= "-Xfatal-warnings")
  val sampleVavr = buildSampleProject("dropwizardVavr", dependenciesVavr).settings(scalacOptions -= "-Xfatal-warnings")
}
