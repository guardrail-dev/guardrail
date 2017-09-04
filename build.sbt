val projectName = "swagger-codegen"
name := projectName
organization in ThisBuild := "com.twilio"
version in ThisBuild := "0.26.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.8"

val akkaVersion = "10.0.10"
val catsVersion = "0.9.0"
val circeVersion = "0.7.0"
val scalatestVersion = "3.0.1"

mainClass in assembly := Some("com.twilio.swagger.codegen.CLI")

lazy val runExample = taskKey[Unit]("Run with example args")
fullRunTask(runExample, Test, "com.twilio.swagger.codegen.CLI", """
  --client --specPath modules/codegen/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName clients --tracing
  --server --specPath modules/codegen/src/main/resources/petstore.json --outputPath modules/sample/src/main/scala --packageName servers --tracing
  --client --specPath modules/codegen/src/main/resources/plain.json --outputPath modules/sample/src/main/scala --packageName tests.dtos
""".replaceAllLiterally("\n", " ").split(' ').filter(_.nonEmpty): _*)

artifact in (Compile, assembly) := {
  val art = (artifact in (Compile, assembly)).value
  art.copy(`classifier` = Some("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

addCommandAlias("cli", "runMain com.twilio.swagger.codegen.CLI")

val resetSample = TaskKey[Unit]("resetSample", "Reset sample module")

resetSample := { "git clean -fdx modules/sample/src modules/sample/target" ! }

addCommandAlias("testSuite", "; test ; resetSample; runExample ; sample/test")

publishMavenStyle := true

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % Test
)

val codegenSettings = Seq(
  resolvers ++= Seq(
    Resolver.url("scalameta", url("http://dl.bintray.com/scalameta/maven"))(Resolver.ivyStylePatterns)
  ),
  libraryDependencies ++= testDependencies ++ Seq(
    "org.scalameta" %% "scalameta" % "1.8.0"
    , "io.swagger" % "swagger-parser" % "1.0.32"
    , "org.typelevel" %% "cats" % catsVersion
  )
  // Dev
  , scalacOptions in ThisBuild ++= Seq(
      "-language:higherKinds",
      "-Xexperimental",
      "-Ybackend:GenBCode",
      "-Ydelambdafy:method",
      "-Xlint:_",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-target:jvm-1.8",
      "-encoding", "utf8"
    )
  , parallelExecution in Test := true
  , fork := true
  , offline := true
)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= testDependencies
  )
  .dependsOn(codegen)

lazy val codegen = (project in file("modules/codegen"))
  .settings(
    (name := s"${projectName}-core") +:
    codegenSettings
  )

lazy val sample = (project in file("modules/sample"))
  .settings(
    codegenSettings
    , initialCommands in console := """
      |import scala.concurrent.ExecutionContext.Implicits.global
      |import scala.meta._
      |import scala.collection.immutable.Seq
      |import java.time._
      |import Common._
      |import scala.concurrent.Await
      |import scala.concurrent.duration._
      |""".stripMargin
    , libraryDependencies ++= Seq(
        "com.typesafe.akka" %% "akka-http" % akkaVersion
      , "io.circe" %% "circe-core" % circeVersion
      , "io.circe" %% "circe-generic" % circeVersion
      , "io.circe" %% "circe-java8" % circeVersion
      , "io.circe" %% "circe-parser" % circeVersion
      , "org.scalatest" %% "scalatest" % scalatestVersion % Test
    )
  )

watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.scala").get
