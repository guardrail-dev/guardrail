name := "guardrail-root"
// Project version is determined by sbt-git based on the most recent tag

git.gitUncommittedChanges := git.gitCurrentTags.value.isEmpty

import dev.guardrail.sbt.Build._
import dev.guardrail.sbt.Dependencies._
import dev.guardrail.sbt.RegressionTests._
import dev.guardrail.sbt.ExampleCase

assembly / mainClass := Some("dev.guardrail.CLI")
assembly / assemblyMergeStrategy := {
  case ".api_description" => MergeStrategy.discard
  case ".options" => MergeStrategy.concat
  case "plugin.properties" => MergeStrategy.discard
  case "plugin.xml" => MergeStrategy.concat
  case "META-INF/eclipse.inf" => MergeStrategy.first
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

WelcomeMessage.welcomeMessage

import scoverage.ScoverageKeys

lazy val runJavaExample: TaskKey[Unit] = taskKey[Unit]("Run java generator with example args")
fullRunTask(
  runJavaExample,
  Test,
  "dev.guardrail.CLI",
  exampleArgs("java").flatten.filter(_.nonEmpty): _*
)

lazy val runScalaExample: TaskKey[Unit] = taskKey[Unit]("Run scala generator with example args")
fullRunTask(
  runScalaExample,
  Test,
  "dev.guardrail.CLI",
  exampleArgs("scala").flatten.filter(_.nonEmpty): _*
)

lazy val runExample: InputKey[Unit] = inputKey[Unit]("Run generators with example args (usage: runExample [language [framework]])")
runExample := Def.inputTaskDyn {
  import complete.DefaultParsers.spaceDelimited

  val args: Seq[String] = spaceDelimited("<arg>").parsed
  val runArgs = args match {
    case language :: framework :: Nil => exampleArgs(language, Some(framework))
    case language :: Nil => exampleArgs(language)
    case Nil => exampleArgs("scala") ++ exampleArgs("java")
  }
  runTask(Test, "dev.guardrail.CLI", runArgs.flatten.filter(_.nonEmpty): _*)
}.evaluated

Compile / assembly / artifact := {
  (Compile / assembly / artifact).value
    .withClassifier(Option("assembly"))
}

addArtifact(Compile / assembly / artifact, assembly)

addCommandAlias("resetSample", "; " ++ (scalaFrameworks ++ javaFrameworks).map(x => s"sample-${x}/clean").mkString(" ; "))

// Deprecated command
addCommandAlias("example", "runtimeSuite")

// Make "cli" not emit unhandled exceptions on exit
run / fork := true

addCommandAlias("cli", "runMain dev.guardrail.CLI")
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
scalacOptions += "-Yrangepos"

publishMavenStyle := true

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.32")
  .dependsOn(guardrail, microsite)
  .aggregate(allDeps, microsite)
  .aggregate(allModules: _*)

lazy val allDeps = (project in file("modules/alldeps"))
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    libraryDependencies ++= akkaProjectDependencies,
    libraryDependencies ++= akkaJacksonProjectDependencies,
    libraryDependencies ++= http4sProjectDependencies,
    libraryDependencies ++= endpointsProjectDependencies,
    libraryDependencies ++= springProjectDependencies,
    libraryDependencies ++= dropwizardProjectDependencies,
    libraryDependencies ++= dropwizardScalaProjectDependencies,
  )

lazy val guardrail = baseModule("guardrail", "guardrail", file("modules/codegen"))
  .dependsOn(core, javaDropwizard, javaSpringMvc, scalaAkkaHttp, scalaEndpoints, scalaHttp4s, scalaDropwizard)

lazy val core = commonModule("core")
  .settings(
    libraryDependencies ++= Seq(
      "com.github.javaparser"       % "javaparser-symbol-solver-core" % "3.22.1",
      "io.swagger.parser.v3"        % "swagger-parser"                % "2.0.27",
    ) ++ Seq(
      "org.scalameta"               %% "scalameta"                    % "4.4.28",
      "org.tpolecat"                %% "atto-core"                    % "0.9.5",
      "org.typelevel"               %% "cats-core"                    % catsVersion,
      "org.typelevel"               %% "cats-kernel"                  % catsVersion,
      "org.typelevel"               %% "cats-free"                    % catsVersion,
      "org.scala-lang.modules"      %% "scala-java8-compat"           % "1.0.0",
    ).map(_.cross(CrossVersion.for3Use2_13)),
  )

lazy val javaSupport = commonModule("java-support")
  .settings(
    libraryDependencies ++= eclipseFormatterDependencies
  )
  .dependsOn(core)

lazy val javaAsyncHttp = commonModule("java-async-http")
  .dependsOn(javaSupport)

lazy val javaDropwizard = commonModule("java-dropwizard")
  .dependsOn(javaSupport, javaAsyncHttp)

lazy val javaSpringMvc = commonModule("java-spring-mvc")
  .dependsOn(javaSupport)

lazy val scalaSupport = commonModule("scala-support")
  .dependsOn(core, javaDropwizard)

lazy val scalaAkkaHttp = commonModule("scala-akka-http")
  .dependsOn(scalaSupport, javaDropwizard)

lazy val scalaEndpoints = commonModule("scala-endpoints")
  .dependsOn(scalaSupport)

lazy val scalaHttp4s = commonModule("scala-http4s")
  .dependsOn(scalaSupport)

lazy val scalaDropwizard = commonModule("scala-dropwizard")
  .dependsOn(javaDropwizard, scalaSupport)

lazy val allModules = Seq[sbt.ProjectReference](
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

lazy val akkaHttpSample = buildSampleProject("akkaHttp", akkaProjectDependencies)

lazy val akkaHttpJacksonSample = buildSampleProject("akkaHttpJackson", akkaJacksonProjectDependencies)

lazy val dropwizardScalaSample = buildSampleProject("dropwizardScala", dropwizardScalaProjectDependencies)

lazy val endpointsSample = buildSampleProject("endpoints", endpointsProjectDependencies)

lazy val http4sSample = buildSampleProject("http4s", http4sProjectDependencies)

val javaSampleSettings = Seq(
    Test / testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    javacOptions ++= Seq(
      "-Xlint:all"
    ),
  )

lazy val dropwizardSample = buildSampleProject("dropwizard", dropwizardProjectDependencies)
  .settings(javaSampleSettings)

lazy val dropwizardVavrSample = buildSampleProject("dropwizardVavr", dropwizardVavrProjectDependencies)
  .settings(javaSampleSettings)

lazy val springMvcSample = buildSampleProject("springMvc", springProjectDependencies)
  .settings(javaSampleSettings)

lazy val microsite = (project in file("modules/microsite"))
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    mdocExtraArguments += "--no-link-hygiene",
  )
  .dependsOn(guardrail)

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
