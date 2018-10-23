val projectName = "guardrail-root"
name := projectName
organization in ThisBuild := "com.twilio"
licenses in ThisBuild += ("MIT", url("http://opensource.org/licenses/MIT"))
// Project version is determined by sbt-git based on the most recent tag
// Publishing information is defined in `bintray.sbt`

enablePlugins(GitVersioning)
git.useGitDescribe := true

crossScalaVersions := Seq("2.11.11", "2.12.6")
scalaVersion in ThisBuild := crossScalaVersions.value.last

scalafmtOnCompile in ThisBuild := true
scalafmtFailTest in ThisBuild := false

val akkaVersion       = "10.0.10"
val catsVersion       = "1.1.0"
val catsEffectVersion = "0.10"
val circeVersion      = "0.9.3"
val http4sVersion     = "0.18.12"
val scalatestVersion  = "3.0.5"

mainClass in assembly := Some("com.twilio.guardrail.CLI")

lazy val runExample: TaskKey[Unit] = taskKey[Unit]("Run with example args")
// TODO: akka.NotUsed should exist in all generated sources, but there are no import verifying tests yet.
fullRunTask(
  runExample,
  Test,
  "com.twilio.guardrail.CLI",
  """
  --defaults --import support.PositiveLong
  --client --specPath modules/sample/src/main/resources/polymorphism.yaml --outputPath modules/sample/src/main/scala --packageName issues.issue43.client.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/polymorphism.yaml --outputPath modules/sample/src/main/scala --packageName issues.issue43.client.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/polymorphism.yaml --outputPath modules/sample/src/main/scala --packageName issues.issue43.server.http4s --framework http4s
  --server --specPath modules/sample/src/main/resources/polymorphism.yaml --outputPath modules/sample/src/main/scala --packageName issues.issue43.server.akkaHttp --framework akka-http
  --client --specPath modules/sample/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName clients.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName clients.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName servers.http4s --framework http4s
  --server --specPath modules/sample/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName servers.akkaHttp --framework akka-http
  --client --specPath modules/sample/src/main/resources/plain.json --outputPath modules/sample/src/main/scala --packageName tests.dtos.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/plain.json --outputPath modules/sample/src/main/scala --packageName tests.dtos.akkaHttp --framework akka-http
  --client --specPath modules/sample/src/main/resources/contentType-textPlain.yaml --outputPath modules/sample/src/main/scala --packageName tests.contentTypes.textPlain.client.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/contentType-textPlain.yaml --outputPath modules/sample/src/main/scala --packageName tests.contentTypes.textPlain.client.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/contentType-textPlain.yaml --outputPath modules/sample/src/main/scala --packageName tests.contentTypes.textPlain.server.http4s --framework http4s
  --server --specPath modules/sample/src/main/resources/contentType-textPlain.yaml --outputPath modules/sample/src/main/scala --packageName tests.contentTypes.textPlain.server.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/raw-response.yaml --outputPath modules/sample/src/main/scala --packageName raw.server.http4s --framework http4s
  --server --specPath modules/sample/src/main/resources/raw-response.yaml --outputPath modules/sample/src/main/scala --packageName raw.server.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/test/resources/server1.yaml --outputPath modules/sample/src/main/scala --packageName tracer.servers.http4s --tracing --framework http4s
  --server --specPath modules/sample/src/test/resources/server1.yaml --outputPath modules/sample/src/main/scala --packageName tracer.servers.akkaHttp --tracing --framework akka-http
  --client --specPath modules/sample/src/test/resources/server1.yaml --outputPath modules/sample/src/main/scala --packageName tracer.clients.http4s --tracing --framework http4s
  --client --specPath modules/sample/src/test/resources/server1.yaml --outputPath modules/sample/src/main/scala --packageName tracer.clients.akkaHttp --tracing --framework akka-http
  --server --specPath modules/sample/src/test/resources/server2.yaml --outputPath modules/sample/src/main/scala --packageName tracer.servers.http4s --tracing --framework http4s
  --server --specPath modules/sample/src/test/resources/server2.yaml --outputPath modules/sample/src/main/scala --packageName tracer.servers.akkaHttp --tracing --framework akka-http
  --client --specPath modules/sample/src/test/resources/server2.yaml --outputPath modules/sample/src/main/scala --packageName tracer.clients.http4s --tracing --framework http4s
  --client --specPath modules/sample/src/test/resources/server2.yaml --outputPath modules/sample/src/main/scala --packageName tracer.clients.akkaHttp --tracing --framework akka-http
  --client --specPath modules/sample/src/main/resources/alias.yaml --outputPath modules/sample/src/main/scala --packageName alias.client.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/alias.yaml --outputPath modules/sample/src/main/scala --packageName alias.client.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/alias.yaml --outputPath modules/sample/src/main/scala --packageName alias.server.http4s --framework http4s
  --server --specPath modules/sample/src/main/resources/alias.yaml --outputPath modules/sample/src/main/scala --packageName alias.server.akkaHttp --framework akka-http
  --client --specPath modules/sample/src/main/resources/edgecases/defaults.yaml --outputPath modules/sample/src/main/scala --packageName edgecases.defaults.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/edgecases/defaults.yaml --outputPath modules/sample/src/main/scala --packageName edgecases.defaults.akkaHttp --framework akka-http
  --client --specPath modules/sample/src/main/resources/custom-header-type.yaml --outputPath modules/sample/src/main/scala --packageName tests.customTypes.customHeader.client.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/custom-header-type.yaml --outputPath modules/sample/src/main/scala --packageName tests.customTypes.customHeader.client.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/custom-header-type.yaml --outputPath modules/sample/src/main/scala --packageName tests.customTypes.customHeader.server.http4s --framework http4s
  --server --specPath modules/sample/src/main/resources/custom-header-type.yaml --outputPath modules/sample/src/main/scala --packageName tests.customTypes.customHeader.server.akkaHttp --framework akka-http
  --client --specPath modules/sample/src/main/resources/formData.yaml --outputPath modules/sample/src/main/scala --packageName form.client.http4s --framework http4s
  --client --specPath modules/sample/src/main/resources/formData.yaml --outputPath modules/sample/src/main/scala --packageName form.client.akkaHttp --framework akka-http
  --server --specPath modules/sample/src/main/resources/formData.yaml --outputPath modules/sample/src/main/scala --packageName form.server.http4s --framework http4s
  --server --specPath modules/sample/src/main/resources/formData.yaml --outputPath modules/sample/src/main/scala --packageName form.server.akkaHttp --framework akka-http
""".replaceAllLiterally("\n", " ").split(' ').filter(_.nonEmpty): _*
)

artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

addCommandAlias("cli", "runMain com.twilio.guardrail.CLI")
addCommandAlias(
  "format",
  "; codegen/scalafmt ; codegen/test:scalafmt ; scalafmt ; codegen/test:scalafmt ; sample/scalafmt ; sample/test:scalafmt"
)

val resetSample = TaskKey[Unit]("resetSample", "Reset sample module")

resetSample := { "git clean -fdx modules/sample/src modules/sample/target" ! }

addCommandAlias("example", "; resetSample ; runExample ; sample/test")
addCommandAlias("testSuite", "; codegen/test ; resetSample; runExample ; sample/test")

addCommandAlias(
  "publishBintray",
  "; set publishTo in codegen := (publishTo in bintray in codegen).value; codegen/publishSigned"
)
addCommandAlias(
  "publishSonatype",
  "; set publishTo in codegen := (sonatypePublishTo in codegen).value; codegen/publishSigned"
)

publishMavenStyle := true

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % Test
)

val codegenSettings = Seq(
  libraryDependencies ++= testDependencies ++ Seq(
    "org.scalameta" %% "scalameta"     % "4.0.0",
    "io.swagger"    % "swagger-parser" % "1.0.34",
    "org.tpolecat"  %% "atto-core"     % "0.6.1",
    "org.typelevel" %% "cats-core"     % catsVersion,
    "org.typelevel" %% "cats-kernel"   % catsVersion,
    "org.typelevel" %% "cats-macros"   % catsVersion,
    "org.typelevel" %% "cats-free"     % catsVersion
  )
  // Dev
  ,
  scalacOptions in ThisBuild ++= Seq(
    "-Ypartial-unification",
    "-language:higherKinds",
    "-Xexperimental",
    "-Ydelambdafy:method",
    "-Xlint:-unused",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-target:jvm-1.8",
    "-encoding",
    "utf8"
  ),
  parallelExecution in Test := true,
  fork := true,
  offline := true
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
