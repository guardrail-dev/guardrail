val projectName = "guardrail-root"
name := projectName
organization in ThisBuild := "com.twilio"
licenses in ThisBuild += ("MIT", url("http://opensource.org/licenses/MIT"))
// Project version is determined by sbt-git based on the most recent tag
// Publishing information is defined in `bintray.sbt`

enablePlugins(GitVersioning)
git.useGitDescribe := true

crossScalaVersions := Seq("2.11.12", "2.12.3")
scalaVersion in ThisBuild := crossScalaVersions.value.last

val akkaVersion       = "10.0.14"
val catsVersion       = "1.4.0"
val catsEffectVersion = "1.0.0"
val circeVersion      = "0.10.1"
val http4sVersion     = "0.19.0"
val scalatestVersion  = "3.0.5"

mainClass in assembly := Some("com.twilio.guardrail.CLI")

// (filename, prefix, tracing)
def sampleResource(name: String): java.io.File = file(s"modules/sample/src/main/resources/${name}")
val exampleCases: List[(java.io.File, String, Boolean, List[String])] = List(
  (sampleResource("alias.yaml"), "alias", false, List.empty),
  (sampleResource("contentType-textPlain.yaml"), "tests.contentTypes.textPlain", false, List.empty),
  (sampleResource("custom-header-type.yaml"), "tests.customTypes.customHeader", false, List.empty),
  (sampleResource("edgecases/defaults.yaml"), "edgecases.defaults", false, List.empty),
  (sampleResource("formData.yaml"), "form", false, List.empty),
  (sampleResource("issues/issue121.yaml"), "issues.issue121", false, List.empty),
  (sampleResource("issues/issue127.yaml"), "issues.issue127", false, List.empty),
  (sampleResource("issues/issue143.yaml"), "issues.issue143", false, List.empty),
  (sampleResource("issues/issue148.yaml"), "issues.issue148", false, List.empty),
  (sampleResource("issues/issue164.yaml"), "issues.issue148", false, List.empty),
  (sampleResource("petstore.json"), "examples", false, List("--import", "support.PositiveLong")),
  (sampleResource("plain.json"), "tests.dtos", false, List.empty),
  (sampleResource("polymorphism.yaml"), "polymorphism", false, List.empty),
  (sampleResource("raw-response.yaml"), "raw", false, List.empty),
  (sampleResource("server1.yaml"), "tracer", true, List.empty),
  (sampleResource("server2.yaml"), "tracer", true, List.empty)
)

val exampleArgs: List[List[String]] = exampleCases
  .foldLeft(List.empty[List[String]])({
    case (acc, (path, prefix, tracing, extra)) =>
      acc ++ (for {
        kind <- List("client", "server")
        frameworkPair <- List(
          ("akka-http", "akkaHttp"),
          ("http4s", "http4s")
        )
        (frameworkName, frameworkPackage) = frameworkPair
        tracingFlag                       = if (tracing) Option("--tracing") else Option.empty[String]
      } yield
        (
          List(s"--${kind}") ++
            List("--specPath", path.toString()) ++
            List("--outputPath", s"modules/sample/src/main/scala/generated") ++
            List("--packageName", s"${prefix}.${kind}.${frameworkPackage}") ++
            List("--framework", frameworkName)
        ) ++ tracingFlag ++ extra)
  })

lazy val runScalaExample: TaskKey[Unit] = taskKey[Unit]("Run scala generator with example args")
fullRunTask(
  runScalaExample,
  Test,
  "com.twilio.guardrail.CLI",
  exampleArgs.flatten.filter(_.nonEmpty): _*
)

artifact in (Compile, assembly) := {
  (artifact in (Compile, assembly)).value
    .withClassifier(Option("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

val resetSample = TaskKey[Unit]("resetSample", "Reset sample module")

resetSample := {
  import scala.sys.process._
  "git clean -fdx modules/sample/src modules/sample/target" !
}

// Deprecated command
addCommandAlias("example", "runtimeSuite")

addCommandAlias("cli", "runMain com.twilio.guardrail.CLI")
addCommandAlias("runtimeSuite", "; resetSample ; runScalaExample ; sample/test")
addCommandAlias("scalaTestSuite", "; codegen/test ; runtimeSuite")
addCommandAlias("format", "; codegen/scalafmt ; codegen/test:scalafmt ; sample/scalafmt ; sample/test:scalafmt")
addCommandAlias("checkFormatting", "; codegen/scalafmtCheck ; codegen/test:scalafmtCheck ; sample/scalafmtCheck ; sample/test:scalafmtCheck")
addCommandAlias("testSuite", "; scalaTestSuite")

addCommandAlias(
  "publishBintray",
  "; set publishTo in codegen := (publishTo in bintray in codegen).value; codegen/publishSigned"
)
addCommandAlias(
  "publishSonatype",
  "; set publishTo in codegen := (sonatypePublishTo in codegen).value; codegen/publishSigned"
)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
addCompilerPlugin("org.spire-math" % "kind-projector"  % "0.9.9" cross CrossVersion.binary)

publishMavenStyle := true

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % Test
)

val excludedWarts = Set(Wart.DefaultArguments, Wart.Product, Wart.Serializable)
val codegenSettings = Seq(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
  wartremoverWarnings in Compile ++= Warts.unsafe.filterNot(w => excludedWarts.exists(_.clazz == w.clazz)),
  wartremoverWarnings in Test := List.empty,
  libraryDependencies ++= testDependencies ++ Seq(
    "org.scalameta"           %% "scalameta"     % "4.1.0",
    "io.swagger.parser.v3"    % "swagger-parser" % "2.0.8",
    "org.tpolecat"            %% "atto-core"     % "0.6.3",
    "org.typelevel"           %% "cats-core"     % catsVersion,
    "org.typelevel"           %% "cats-kernel"   % catsVersion,
    "org.typelevel"           %% "cats-macros"   % catsVersion,
    "org.typelevel"           %% "cats-free"     % catsVersion
  ),
  scalacOptions in ThisBuild ++= Seq(
    "-Ypartial-unification",
    "-Ydelambdafy:method",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-encoding",
    "utf8"
  ) ++ (if (scalaVersion.value.startsWith("2.11.")) {
          List("-Xexperimental", "-Xlint:_")
        } else {
          List("-Xlint:-unused,_")
        }),
  parallelExecution in Test := true,
  fork := true
)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= testDependencies,
    skip in publish := true
  )
  .dependsOn(codegen)

lazy val codegen = (project in file("modules/codegen"))
  .settings(
    (name := "guardrail") +:
      codegenSettings,
    scalacOptions += "-language:higherKinds",
    bintrayRepository := {
      if (isSnapshot.value) "snapshots"
      else "releases"
    },
    bintrayPackageLabels := Seq(
      "codegen",
      "openapi",
      "swagger"
    ),
    description := "Principled code generation for Scala services from OpenAPI specifications",
    homepage := Some(url("https://github.com/twilio/guardrail")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/twilio/guardrail"),
        "scm:git@github.com:twilio/guardrail.git"
      )
    ),
    developers := List(
      Developer(
        id = "blast_hardcheese",
        name = "Devon Stewart",
        email = "blast@hardchee.se",
        url = url("http://hardchee.se/")
      )
    )
  )

lazy val sample = (project in file("modules/sample"))
  .settings(
    codegenSettings,
    initialCommands in console := """
      |import cats._
      |import cats.data.EitherT
      |import cats.implicits._
      |import cats.effect.IO
      |import io.circe._
      |import java.time._
      |import org.http4s._
      |import org.http4s.client._
      |import org.http4s.client.blaze._
      |import org.http4s.dsl._
      |import org.http4s.multipart._
      |import scala.concurrent.Await
      |import scala.concurrent.ExecutionContext.Implicits.global
      |import scala.concurrent.duration._
      |import scala.meta._
      |""".stripMargin,
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http"           % akkaVersion,
      "com.typesafe.akka" %% "akka-http-testkit"   % akkaVersion,
      "io.circe"          %% "circe-core"          % circeVersion,
      "io.circe"          %% "circe-generic"       % circeVersion,
      "io.circe"          %% "circe-java8"         % circeVersion,
      "io.circe"          %% "circe-parser"        % circeVersion,
      "org.http4s"        %% "http4s-blaze-client" % http4sVersion,
      "org.http4s"        %% "http4s-blaze-server" % http4sVersion,
      "org.http4s"        %% "http4s-circe"        % http4sVersion,
      "org.http4s"        %% "http4s-dsl"          % http4sVersion,
      "org.scalatest"     %% "scalatest"           % scalatestVersion % Test,
      "org.typelevel"     %% "cats-core"           % catsVersion,
      "org.typelevel"     %% "cats-effect"         % catsEffectVersion
    ),
    skip in publish := true,
    scalafmtOnCompile := false
  )

watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.scala").get

logBuffered in Test := false
