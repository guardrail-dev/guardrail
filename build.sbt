name := "guardrail-root"
// Project version is determined by sbt-git based on the most recent tag

git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty

import dev.guardrail.sbt.Build._
import dev.guardrail.sbt.Dependencies._
import dev.guardrail.sbt.RegressionTests._
import dev.guardrail.sbt.ExampleCase
import dev.guardrail.sbt.modules

onLoadMessage := WelcomeMessage.welcomeMessage((guardrail / version).value)

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

addCommandAlias("runtimeAkkaHttpSuite", "; resetSample ; runExample scala akka-http ; sample-akkaHttp / test")

addCommandAlias("resetSample", "; " ++ (scalaFrameworks ++ javaFrameworks).map(x => s"sample-${x}/clean").mkString(" ; "))

// Deprecated command
addCommandAlias("example", "runtimeSuite")

// Make "cli" not emit unhandled exceptions on exit
run / fork := true

addCommandAlias("cli", "runMain dev.guardrail.cli.CLI")
addCommandAlias("runtimeScalaSuite", "; resetSample ; runScalaExample ; " + scalaFrameworks.map(x => s"sample-${x}/test").mkString("; "))
addCommandAlias("runtimeJavaSuite", "; resetSample ; runJavaExample ; " + javaFrameworks.map(x => s"sample-${x}/test").mkString("; "))
addCommandAlias("runtimeSuite", "; runtimeScalaSuite ; runtimeJavaSuite")
addCommandAlias("scalaTestSuite", "; guardrail/test ; runtimeScalaSuite")
addCommandAlias("javaTestSuite", "; guardrail/test ; runtimeJavaSuite")
addCommandAlias("format", "; guardrail/scalafmt ; guardrail/test:scalafmt ; " + scalaFrameworks.map(x => s"sample-${x}/scalafmt ; sample-${x}/test:scalafmt").mkString("; "))
addCommandAlias("checkFormatting", "; guardrail/scalafmtCheck ; guardrail/Test/scalafmtCheck ; " + scalaFrameworks.map(x => s"sample-${x}/scalafmtCheck ; sample-${x}/Test/scalafmtCheck").mkString("; "))
addCommandAlias("testSuite", "; scalaTestSuite ; javaTestSuite; microsite/compile")

addCommandAlias(
  "publishSonatype",
  "; set publishTo in guardrail := (sonatypePublishToBundle in guardrail).value; guardrail/publish"
)
addCommandAlias(
  "publishLocal",
  "; package ; guardrail/publishLocal"
)
addCommandAlias(
  "publishM2",
  "; package ; guardrail/publishM2"
)

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
  .internalModuleDep(guardrail)
  .internalModuleDep(microsite)
  .internalModuleDep(cli)
  .aggregate(allDeps, microsite)
  .aggregate(
    cli,
    core,
    guardrail,

    javaSupport,
    javaAsyncHttp,
    javaDropwizard,
    javaSpringMvc,

    scalaSupport,
    scalaAkkaHttp,
    scalaEndpoints,
    scalaHttp4s,
    scalaDropwizard,
  )

lazy val allDeps = modules.allDeps.project
  .settings(publish / skip := true)
  .settings(crossScalaVersions := crossScalaVersions.value.filter(_.startsWith("2.12")))

lazy val guardrail = modules.guardrail.project
  .providedModuleDep(javaDropwizard, guardrailJavaDropwizardVersion)
  .providedModuleDep(javaSpringMvc, guardrailJavaSpringMvcVersion)
  .providedModuleDep(javaSupport, guardrailJavaSupportVersion)
  .providedModuleDep(javaAsyncHttp, guardrailJavaAsyncHttpVersion)
  .providedModuleDep(scalaAkkaHttp, guardrailScalaAkkaHttpVersion)
  .providedModuleDep(scalaDropwizard, guardrailScalaDropwizardVersion)
  .providedModuleDep(scalaEndpoints, guardrailScalaEndpointsVersion)
  .providedModuleDep(scalaHttp4s, guardrailScalaHttp4sVersion)
  .providedModuleDep(scalaSupport, guardrailScalaSupportVersion)

lazy val samples = (project in file("modules/samples"))
  .settings(publish / skip := true)
  .aggregate(
    dropwizardSample,
    dropwizardVavrSample,
    javaSpringMvcSample,
    scalaAkkaHttpJacksonSample,
    scalaAkkaHttpSample,
    scalaDropwizardSample,
    scalaEndpointsSample,
    scalaHttp4sSample
  )

lazy val core = modules.core.project

lazy val cli = modules.cli.project
  .providedModuleDep(guardrail, guardrailVersion)

lazy val javaSupport = modules.javaSupport.project
  .directModuleDep(core, guardrailCoreVersion)

lazy val javaAsyncHttp = modules.javaAsyncHttp.project
  .directModuleDep(javaSupport, guardrailJavaSupportVersion)

lazy val dropwizardSample = modules.javaDropwizard.sample
  .settings(javaSampleSettings)
lazy val dropwizardVavrSample = modules.javaDropwizard.sampleVavr
  .settings(javaSampleSettings)
lazy val javaDropwizard = modules.javaDropwizard.project
  .directModuleDep(javaSupport, guardrailJavaSupportVersion)
  .directModuleDep(javaAsyncHttp, guardrailJavaAsyncHttpVersion)

lazy val javaSpringMvcSample = modules.javaSpringMvc.sample
  .settings(javaSampleSettings)
lazy val javaSpringMvc = modules.javaSpringMvc.project
  .directModuleDep(javaSupport, guardrailJavaSupportVersion)

lazy val scalaSupport = modules.scalaSupport.project
  .directModuleDep(core, guardrailCoreVersion)

lazy val scalaAkkaHttpSample = modules.scalaAkkaHttp.sample
lazy val scalaAkkaHttpJacksonSample = modules.scalaAkkaHttp.sampleJackson
lazy val scalaAkkaHttp = modules.scalaAkkaHttp.project
  .directModuleDep(scalaSupport, guardrailScalaSupportVersion)

lazy val scalaEndpointsSample = modules.scalaEndpoints.sample
lazy val scalaEndpoints = modules.scalaEndpoints.project
  .directModuleDep(scalaSupport, guardrailScalaSupportVersion)

lazy val scalaHttp4sSample = modules.scalaHttp4s.sample
lazy val scalaHttp4s = modules.scalaHttp4s.project
  .directModuleDep(scalaSupport, guardrailScalaSupportVersion)

lazy val scalaDropwizardSample = modules.scalaDropwizard.sample
lazy val scalaDropwizard = modules.scalaDropwizard.project
  .directModuleDep(scalaSupport, guardrailScalaSupportVersion)

lazy val microsite = baseModule("microsite", "microsite", file("modules/microsite"))
  .settings(
    publish / skip := true,
    mdocExtraArguments += "--no-link-hygiene",
    scalacOptions -= "-Xfatal-warnings"
  )
  .internalModuleDep(guardrail)

watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.scala").get
watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.java").get

lazy val githubMatrixSettings = taskKey[String]("Prints JSON value expected by the Scala CI matrix build: [{ version: ..., bincompat: ... }]")

githubMatrixSettings := {
  (guardrail/crossScalaVersions).value
    .map(v => (v, v.split('.').take(2).mkString(".")))
    .map({ case (version, bincompat) => s"""{"version":"${version}","bincompat":"${bincompat}"}""" })
    .mkString("[", ",", "]")
}

Test / logBuffered := false
