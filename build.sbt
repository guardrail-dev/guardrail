name := "guardrail-root"
// Project version is determined by sbt-git based on the most recent tag

git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty

import dev.guardrail.sbt.Build._
import dev.guardrail.sbt.Dependencies
import dev.guardrail.sbt.RegressionTests._
import dev.guardrail.sbt.ExampleCase
import dev.guardrail.sbt.modules

onLoadMessage := WelcomeMessage.welcomeMessage((core / version).value)

import scoverage.ScoverageKeys

lazy val runJavaExample: TaskKey[Unit] = taskKey[Unit]("Run java generator with example args")
fullRunTask(
  runJavaExample,
  Test,
  "dev.guardrail.cli.CLI",
  exampleArgs("java").flatten.filter(_.nonEmpty): _*
)

lazy val runScalaExample: TaskKey[Unit] = taskKey[Unit]("Run scala generator with example args")
fullRunTask(
  runScalaExample,
  Test,
  "dev.guardrail.cli.CLI",
  exampleArgs("scala").flatten.filter(_.nonEmpty): _*
)

lazy val runExample: InputKey[Unit] = inputKey[Unit]("Run generators with example args (usage: runExample [language [framework]])")
runExample := Def.inputTaskDyn {
  import complete.DefaultParsers.spaceDelimited

  val args: Seq[String] = spaceDelimited("<arg>").parsed
  val runArgs: List[List[List[String]]] = args match {
    case language :: framework :: Nil => List(exampleArgs(language, Some(framework)))
    case language :: Nil => List(exampleArgs(language))
    case Nil => List(exampleArgs("scala"), exampleArgs("java"))
  }
  Def.sequential(
    runArgs.map(args => runTask(Test, "dev.guardrail.cli.CLI", args.flatten.filter(_.nonEmpty): _*))
  )
}.evaluated

// Make "cli" not emit unhandled exceptions on exit
Test / fork := true
run / fork := true

addCommandAlias("runtimeAkkaHttpSuite", "; resetSample ; runExample scala akka-http ; sample-akkaHttp / test")

addCommandAlias("resetSample", "; " ++ (scalaFrameworks ++ javaFrameworks).map(x => s"sample-${x.projectName}/clean").mkString(" ; "))

// Deprecated command
addCommandAlias("example", "runtimeSuite")

addCommandAlias("cli", "runMain dev.guardrail.cli.CLI")
addCommandAlias("runtimeScalaSuite", "; resetSample ; runScalaExample ; " + scalaFrameworks.map(x => s"sample-${x.projectName}/test").mkString("; "))
addCommandAlias("runtimeJavaSuite", "; resetSample ; runJavaExample ; " + javaFrameworks.map(x => s"sample-${x.projectName}/test").mkString("; "))
addCommandAlias("runtimeSuite", "; runtimeScalaSuite ; runtimeJavaSuite")
addCommandAlias("scalaTestSuite", "; test ; runtimeScalaSuite")
addCommandAlias("javaTestSuite", "; test ; runtimeJavaSuite")
addCommandAlias("format", "; scalafmtAll ; " + (scalaFrameworks ++ javaFrameworks).map(x => s"sample-${x.projectName}/scalafmtAll").mkString("; "))
addCommandAlias("checkFormatting", "; scalafmtCheckAll ; " + (scalaFrameworks ++ javaFrameworks).map(x => s"sample-${x.projectName}/scalafmtCheckAll").mkString("; "))
addCommandAlias("testSuite", "; test ; runtimeScalaSuite ; runtimeJavaSuite ; microsite/compile")
addCommandAlias("compileSamples", (scalaFrameworks ++ javaFrameworks).map(x => s"sample-${x.projectName}/Test/compile").mkString("; "))

resolvers += Resolver.sonatypeRepo("releases")

publishMavenStyle := true

val javaSampleSettings = Seq(
    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    javacOptions ++= Seq(
      "-Xlint:all"
    ),
  )

lazy val root = modules.root.project
  .settings(publish / skip := true)
  .settings(libraryDependencies ++= Dependencies.testDependencies)
  .settings(evictionErrorLevel := Level.Debug)  // Suppress "found version conflict(s) in library dependencies; some are suspected to be binary incompatible" in aggregate project
  .dependsOn(core % "compile->compile;test->test")
  .dependsOn(javaSupport, scalaSupport)
  .dependsOn(javaAsyncHttp, javaDropwizard, javaSpringMvc)
  .dependsOn(scalaAkkaHttp, scalaDropwizard, scalaHttp4s)
  .dependsOn(cli)
  .aggregate(allDeps, microsite)
  .aggregate(
    cli,
    core,

    javaSupport,
    javaAsyncHttp,
    javaDropwizard,
    javaSpringMvc,

    scalaSupport,
    scalaAkkaHttp,
    scalaHttp4s,
    scalaDropwizard,
  )

lazy val allDeps = modules.allDeps.project
  .settings(publish / skip := true)
  .settings(crossScalaVersions := crossScalaVersions.value.filter(_.startsWith("2.12")))

lazy val samples = (project in file("modules/samples"))
  .settings(publish / skip := true)
  .aggregate(
    dropwizardSample,
    dropwizardVavrSample,
    javaSpringMvcSample,
    scalaAkkaHttpJacksonSample,
    scalaAkkaHttpSample,
    scalaDropwizardSample,
    scalaHttp4sSample,
    scalaHttp4sSampleV0_22
  )

lazy val core = modules.core.project
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val cli = modules.cli.project
  .customDependsOn(core)
  .settings(run / fork := true)
  .dependsOn(scalaSupport % "test->compile")
  .dependsOn(scalaAkkaHttp % "test->compile")
  .dependsOn(scalaHttp4s % "test->compile")

lazy val javaSupport = modules.javaSupport.project
  .customDependsOn(core)

lazy val javaAsyncHttp = modules.javaAsyncHttp.project
  .customDependsOn(javaSupport)

lazy val dropwizardSample = modules.javaDropwizard.sample
  .settings(javaSampleSettings)
lazy val dropwizardVavrSample = modules.javaDropwizard.sampleVavr
  .settings(javaSampleSettings)
lazy val javaDropwizard = modules.javaDropwizard.project
  .customDependsOn(javaSupport)
  .dependsOn(javaAsyncHttp % "test->compile")

lazy val javaSpringMvcSample = modules.javaSpringMvc.sample
  .settings(javaSampleSettings)
lazy val javaSpringMvc = modules.javaSpringMvc.project
  .customDependsOn(javaSupport)

lazy val scalaSupport = modules.scalaSupport.project
  .customDependsOn(core)

lazy val scalaAkkaHttpSample = modules.scalaAkkaHttp.sample
lazy val scalaAkkaHttpJacksonSample = modules.scalaAkkaHttp.sampleJackson
lazy val scalaAkkaHttp = modules.scalaAkkaHttp.project
  .customDependsOn(scalaSupport)

lazy val scalaHttp4sSampleV0_22 = modules.scalaHttp4s.sampleV0_22
lazy val scalaHttp4sSample = modules.scalaHttp4s.sample
lazy val scalaHttp4s = modules.scalaHttp4s.project
  .customDependsOn(scalaSupport)

lazy val scalaDropwizardSample = modules.scalaDropwizard.sample
lazy val scalaDropwizard = modules.scalaDropwizard.project
  .customDependsOn(scalaSupport)

lazy val microsite = baseModule("microsite", "microsite", file("modules/microsite"))
  .settings(
    publish / skip := true,
    mdocExtraArguments += "--no-link-hygiene",
    scalacOptions -= "-Xfatal-warnings"
  )
  .dependsOn(scalaAkkaHttp)
  .dependsOn(scalaHttp4s)

watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.scala").get
watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.java").get

lazy val githubMatrixSettings = taskKey[String]("Prints JSON value expected by the Scala CI matrix build: [{ version: ..., bincompat: ... }]")

githubMatrixSettings := {
  (root/crossScalaVersions).value
    .map(v => (v, v.split('.').take(2).mkString(".")))
    .map({ case (version, bincompat) => s"""{"version":"${version}","bincompat":"${bincompat}"}""" })
    .mkString("[", ",", "]")
}

Test / logBuffered := false
