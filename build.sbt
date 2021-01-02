import complete.DefaultParsers._

name := "guardrail-root"
// Project version is determined by sbt-git based on the most recent tag

enablePlugins(GitVersioning)
git.useGitDescribe := true

val akkaVersion            = "2.6.10"
val akkaHttpVersion        = "10.2.1"
val catsVersion            = "2.1.1"
val catsEffectVersion      = "2.2.0"
val circeVersion           = "0.13.0"
val http4sVersion          = "0.21.15"
val scalacheckVersion      = "1.15.2"
val scalatestVersion       = "3.2.3"
val scalatestPlusVersion   = "3.1.0.0-RC2"
val javaparserVersion      = "3.18.0"
val endpointsVersion       = "0.8.0"
val ahcVersion             = "2.8.1"
val dropwizardVersion      = "1.3.28"
val dropwizardScalaVersion = "1.3.7-1"
val jerseyVersion          = "2.25.1"
val kindProjectorVersion   = "0.11.2"
val jaxbApiVersion         = "2.3.1"
val javaxAnnotationVersion = "1.3.2"
val springBootVersion      = "2.3.7.RELEASE"
val jacksonVersion         = "2.11.4"
val hibernateVersion       = "7.0.0.Final"
val javaxElVersion         = "3.0.0"
val javaxValidationVersion = "2.0.1.Final"
val vavrVersion            = "0.10.3"
val dropwizardVavrVersion  = "1.3.0-4"

mainClass in assembly := Some("com.twilio.guardrail.CLI")
assemblyMergeStrategy in assembly := {
  case ".api_description" => MergeStrategy.discard
  case ".options" => MergeStrategy.concat
  case "plugin.properties" => MergeStrategy.discard
  case "plugin.xml" => MergeStrategy.concat
  case "META-INF/eclipse.inf" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

val exampleFrameworkSuites = Map(
  "scala" -> List(
    ExampleFramework("akka-http", "akkaHttp"),
    ExampleFramework("endpoints", "endpoints", List("client")),
    ExampleFramework("http4s", "http4s"),
    ExampleFramework("akka-http-jackson", "akkaHttpJackson"),
    ExampleFramework("dropwizard", "dropwizardScala", List("server")),
  ),
  "java" -> List(
    ExampleFramework("dropwizard", "dropwizard"),
    ExampleFramework("dropwizard-vavr", "dropwizardVavr", modules = List("java-vavr", "jackson", "async-http-client", "dropwizard")),
    ExampleFramework("spring-mvc", "springMvc", List("server"))
  )
)


val scalaFrameworks = exampleFrameworkSuites("scala").map(_.projectName)
val javaFrameworks = exampleFrameworkSuites("java").map(_.projectName)

import scoverage.ScoverageKeys

import com.twilio.guardrail.sbt.ExampleCase
def sampleResource(name: String): java.io.File = file(s"modules/sample/src/main/resources/${name}")
val exampleCases: List[ExampleCase] = List(
  ExampleCase(sampleResource("additional-properties.yaml"), "additionalProperties"),
  ExampleCase(sampleResource("alias.yaml"), "alias"),
  ExampleCase(sampleResource("char-encoding/char-encoding-request-stream.yaml"), "charEncoding.requestStream").frameworks("java" -> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("dropwizard")),
  ExampleCase(sampleResource("char-encoding/char-encoding-response-stream.yaml"), "charEncoding.responseStream").frameworks("java"-> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("dropwizard")),
  ExampleCase(sampleResource("contentType-textPlain.yaml"), "tests.contentTypes.textPlain"),
  ExampleCase(sampleResource("custom-header-type.yaml"), "tests.customTypes.customHeader"),
  ExampleCase(sampleResource("date-time.yaml"), "dateTime"),
  ExampleCase(sampleResource("edgecases/defaults.yaml"), "edgecases.defaults"),
  ExampleCase(sampleResource("invalid-characters.yaml"), "invalidCharacters").frameworks("java" -> Set("dropwizard", "dropwizard-vavr")),
  ExampleCase(sampleResource("formData.yaml"), "form"),
  ExampleCase(sampleResource("issues/issue45.yaml"), "issues.issue45"),
  ExampleCase(sampleResource("issues/issue121.yaml"), "issues.issue121"),
  ExampleCase(sampleResource("issues/issue127.yaml"), "issues.issue127"),
  ExampleCase(sampleResource("issues/issue143.yaml"), "issues.issue143"),
  ExampleCase(sampleResource("issues/issue148.yaml"), "issues.issue148"),
  ExampleCase(sampleResource("issues/issue164.yaml"), "issues.issue164"),
  ExampleCase(sampleResource("issues/issue184.yaml"), "issues.issue184"),
  ExampleCase(sampleResource("issues/issue179.yaml"), "issues.issue179"),
  ExampleCase(sampleResource("issues/issue215.yaml"), "issues.issue215"),
  ExampleCase(sampleResource("issues/issue218.yaml"), "issues.issue218"),
  ExampleCase(sampleResource("issues/issue222.yaml"), "issues.issue222"),
  ExampleCase(sampleResource("issues/issue223.yaml"), "issues.issue223"),
  ExampleCase(sampleResource("issues/issue249.yaml"), "issues.issue249"),
  ExampleCase(sampleResource("issues/issue264.yaml"), "issues.issue264"),
  ) ++ {
    val options = List[Option[String]](None, Some("legacy"), Some("optional"), Some("required-nullable"))
    for {
      a <- options
      b <- options
    } yield {
      val (suffix, opts): (String, Seq[String]) = (a, b) match {
        case (None, None) => ("", Seq.empty)
        case (a, b) =>
          (
            s".${a.getOrElse("default")}${b.getOrElse("default")}".split("-").mkString("_"),
            a.toSeq.flatMap(Seq("--optional-encode-as", _)) ++ b.toSeq.flatMap(Seq("--optional-decode-as", _))
          )
      }
      ExampleCase(sampleResource("issues/issue315.yaml"), s"issues.issue315${suffix}")
        .args(opts: _*)
    }
  } ++ List(
  ExampleCase(sampleResource("issues/issue325.yaml"), "issues.issue325"),
  ExampleCase(sampleResource("issues/issue351.yaml"), "issues.issue351"),
  ExampleCase(sampleResource("issues/issue357.yaml"), "issues.issue357"),
  ExampleCase(sampleResource("issues/issue364.yaml"), "issues.issue364").args("--dtoPackage", "some.thing"),
  ExampleCase(sampleResource("issues/issue389.yaml"), "issues.issue389"),
  ExampleCase(sampleResource("issues/issue405.yaml"), "issues.issue405"),
  ExampleCase(sampleResource("issues/issue440.yaml"), "issues.issue440"),
  ExampleCase(sampleResource("issues/issue455.yaml"), "issues.issue455"),
  ExampleCase(sampleResource("issues/issue622.yaml"), "issues.issue622"),
  ExampleCase(sampleResource("multipart-form-data.yaml"), "multipartFormData"),
  ExampleCase(sampleResource("petstore.json"), "examples").args("--import", "examples.support.PositiveLong"),
  // ExampleCase(sampleResource("petstore-openapi-3.0.2.yaml"), "examples.petstore.openapi302").args("--import", "examples.support.PositiveLong"),
  ExampleCase(sampleResource("plain.json"), "tests.dtos"),
  ExampleCase(sampleResource("polymorphism.yaml"), "polymorphism"),
  ExampleCase(sampleResource("polymorphism-mapped.yaml"), "polymorphismMapped"),
  ExampleCase(sampleResource("polymorphism-nested.yaml"), "polymorphismNested"),
  ExampleCase(sampleResource("raw-response.yaml"), "raw"),
  ExampleCase(sampleResource("redaction.yaml"), "redaction"),
  ExampleCase(sampleResource("server1.yaml"), "tracer").args("--tracing"),
  ExampleCase(sampleResource("server2.yaml"), "tracer").args("--tracing"),
  ExampleCase(sampleResource("pathological-parameters.yaml"), "pathological"),
  ExampleCase(sampleResource("response-headers.yaml"), "responseHeaders"),
  ExampleCase(sampleResource("random-content-types.yaml"), "randomContentTypes").frameworks("java" -> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("http4s", "dropwizard")),
  ExampleCase(sampleResource("binary.yaml"), "binary").frameworks("java" -> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("http4s")),
  ExampleCase(sampleResource("conflicting-names.yaml"), "conflictingNames"),
  ExampleCase(sampleResource("base64.yaml"), "base64").frameworks("scala" -> scalaFrameworks.toSet),
  ExampleCase(sampleResource("server1.yaml"), "customExtraction").args("--custom-extraction").frameworks("scala" -> Set("akka-http", "http4s"))
)

def exampleArgs(language: String, framework: Option[String] = None): List[List[String]] = exampleCases
  .foldLeft(List[List[String]](List(language)))({
    case (acc, ExampleCase(path, prefix, extra, onlyFrameworks)) =>
      acc ++ (for {
        frameworkSuite <- exampleFrameworkSuites(language).filter(efs => framework.forall(_ == efs.name))
        ExampleFramework(frameworkName, frameworkPackage, kinds, modules) = frameworkSuite
        if onlyFrameworks.forall(_.exists({ case (onlyLanguage, onlyFrameworks) => onlyLanguage == language && onlyFrameworks.contains(frameworkName) }))
        kind <- kinds
        filteredExtra = extra.filterNot(if (language == "java" || (language == "scala" && frameworkName == "dropwizard")) _ == "--tracing" else Function.const(false) _)
      } yield
        (
          List(s"--${kind}") ++
            List("--specPath", path.toString()) ++
            List("--outputPath", s"modules/sample-${frameworkPackage}/target/generated") ++
            List("--packageName", s"${prefix}.${kind}.${frameworkPackage}") ++
            List("--framework", frameworkName) ++
            modules.flatMap(module => List("--module", module))
        ) ++ filteredExtra)
  })

lazy val runJavaExample: TaskKey[Unit] = taskKey[Unit]("Run java generator with example args")
fullRunTask(
  runJavaExample,
  Test,
  "com.twilio.guardrail.CLI",
  exampleArgs("java").flatten.filter(_.nonEmpty): _*
)

lazy val runScalaExample: TaskKey[Unit] = taskKey[Unit]("Run scala generator with example args")
fullRunTask(
  runScalaExample,
  Test,
  "com.twilio.guardrail.CLI",
  exampleArgs("scala").flatten.filter(_.nonEmpty): _*
)

lazy val runExample: InputKey[Unit] = inputKey[Unit]("Run generators with example args (usage: runExample [language] [framework])")
runExample := Def.inputTaskDyn {
  val args: Seq[String] = spaceDelimited("<arg>").parsed
  val runArgs = args match {
    case language :: framework :: Nil => exampleArgs(language, Some(framework))
    case language :: Nil => exampleArgs(language)
    case Nil => exampleArgs("scala") ++ exampleArgs("java")
  }
  runTask(Test, "com.twilio.guardrail.CLI", runArgs.flatten.filter(_.nonEmpty): _*)
}.evaluated

artifact in (Compile, assembly) := {
  (artifact in (Compile, assembly)).value
    .withClassifier(Option("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

addCommandAlias("resetSample", "; " ++ (scalaFrameworks ++ javaFrameworks).map(x => s"${x}Sample/clean").mkString(" ; "))

// Deprecated command
addCommandAlias("example", "runtimeSuite")

// Make "cli" not emit unhandled exceptions on exit
fork in run := true

addCommandAlias("cli", "runMain com.twilio.guardrail.CLI")
addCommandAlias("runtimeScalaSuite", "; resetSample ; runScalaExample ; " + scalaFrameworks.map(x => s"${x}Sample/test").mkString("; "))
addCommandAlias("runtimeJavaSuite", "; resetSample ; runJavaExample ; " + javaFrameworks.map(x => s"${x}Sample/test").mkString("; "))
addCommandAlias("runtimeSuite", "; runtimeScalaSuite ; runtimeJavaSuite")
addCommandAlias("scalaTestSuite", "; codegen/test ; runtimeScalaSuite")
addCommandAlias("javaTestSuite", "; codegen/test ; runtimeJavaSuite")
addCommandAlias("format", "; codegen/scalafmt ; codegen/test:scalafmt ; " + scalaFrameworks.map(x => s"${x}Sample/scalafmt ; ${x}Sample/test:scalafmt").mkString("; "))
addCommandAlias("checkFormatting", "; codegen/scalafmtCheck ; codegen/test:scalafmtCheck ; " + scalaFrameworks.map(x => s"${x}Sample/scalafmtCheck ; ${x}Sample/test:scalafmtCheck").mkString("; "))
addCommandAlias("testSuite", "; scalaTestSuite ; javaTestSuite; microsite/compile")

addCommandAlias(
  "publishSonatype",
  "; set publishTo in codegen := (sonatypePublishToBundle in codegen).value; codegen/publish"
)
addCommandAlias(
  "publishLocal",
  "; package ; codegen/publishLocal"
)
addCommandAlias(
  "publishM2",
  "; package ; codegen/publishM2"
)

resolvers += Resolver.sonatypeRepo("releases")
scalacOptions += "-Yrangepos"

publishMavenStyle := true

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % Test,
  "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalatestPlusVersion % Test
)

def ifScalaVersion[A](minorPred: Int => Boolean = _ => true)(value: List[A]): Def.Initialize[Seq[A]] = Def.setting {
  scalaVersion.value.split('.') match {
    case Array("2", minor, bugfix) if minorPred(minor.toInt) => value
    case _                                                   => Nil
  }
}

val commonSettings = Seq(
  organization := "com.twilio",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),

  crossScalaVersions := Seq("2.12.12", "2.13.4"),
  scalaVersion := "2.12.12",

  scalacOptions ++= Seq(
    "-Ydelambdafy:method",
    "-Yrangepos",
    // "-Ywarn-unused-import",  // TODO: Enable this! https://github.com/twilio/guardrail/pull/282
    "-feature",
    "-unchecked",
    "-deprecation",
    "-encoding",
    "utf8"
  ),
  scalacOptions ++= ifScalaVersion(_ <= 11)(List("-Xexperimental", "-Xlint:-missing-interpolator,_")).value,
  scalacOptions ++= ifScalaVersion(_ >= 12)(List("-Xlint:-unused,-missing-interpolator,_")).value,
  scalacOptions ++= ifScalaVersion(_ == 12)(List("-Ypartial-unification", "-Ywarn-unused-import")).value,
  scalacOptions ++= ifScalaVersion(_ >= 13)(List("-Ywarn-unused:imports")).value,
  parallelExecution in Test := true,
  addCompilerPlugin("org.typelevel" % "kind-projector"  % kindProjectorVersion cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  addCompilerPlugin(scalafixSemanticdb),
)

val excludedWarts = Set(Wart.DefaultArguments, Wart.Product, Wart.Serializable, Wart.Any)
val codegenSettings = Seq(
  ScoverageKeys.coverageExcludedPackages := "<empty>;com.twilio.guardrail.terms.*;com.twilio.guardrail.protocol.terms.*",
  wartremoverWarnings in Compile ++= Warts.unsafe.filterNot(w => excludedWarts.exists(_.clazz == w.clazz)),
  wartremoverWarnings in Test := List.empty,
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(skip in publish := true)
  .dependsOn(codegen, microsite)
  .aggregate(allDeps, codegen, microsite, endpointsDependencies)

lazy val allDeps = (project in file("modules/alldeps"))
  .settings(commonSettings)
  .settings(
    skip in publish := true,
    libraryDependencies ++= akkaProjectDependencies,
    libraryDependencies ++= akkaJacksonProjectDependencies,
    libraryDependencies ++= http4sProjectDependencies,
    libraryDependencies ++= springProjectDependencies,
    libraryDependencies ++= dropwizardProjectDependencies,
    libraryDependencies ++= dropwizardScalaProjectDependencies,
  )

lazy val codegen = (project in file("modules/codegen"))
  .settings(commonSettings)
  .settings(
    name := "guardrail"
  )
  .settings(codegenSettings)
  .settings(libraryDependencies ++= testDependencies)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta"               %% "scalameta"                    % "4.4.1",
      "com.github.javaparser"       % "javaparser-symbol-solver-core" % javaparserVersion,
      "org.eclipse.jdt"             % "org.eclipse.jdt.core"          % "3.24.0",
      "org.eclipse.platform"        % "org.eclipse.equinox.app"       % "1.5.0",
      "io.swagger.parser.v3"        % "swagger-parser"                % "2.0.24",
      "org.tpolecat"                %% "atto-core"                    % "0.8.0",
      "org.typelevel"               %% "cats-core"                    % catsVersion,
      "org.typelevel"               %% "cats-kernel"                  % catsVersion,
      "org.typelevel"               %% "cats-macros"                  % catsVersion,
      "org.typelevel"               %% "cats-free"                    % catsVersion,
      "org.scala-lang.modules"      %% "scala-java8-compat"           % "0.9.1",
    ),
    scalacOptions ++= List(
      "-language:higherKinds",
      "-Xlint:_,-missing-interpolator"
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

val akkaProjectDependencies = Seq(
  "javax.annotation"  %  "javax.annotation-api" % javaxAnnotationVersion, // for jdk11
  "javax.xml.bind"    %  "jaxb-api"             % jaxbApiVersion, // for jdk11
  "com.typesafe.akka" %% "akka-http"            % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-testkit"    % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream"          % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit"         % akkaVersion,
  "io.circe"          %% "circe-core"           % circeVersion,
  "io.circe"          %% "circe-jawn"           % circeVersion,
  "io.circe"          %% "circe-parser"         % circeVersion,
  "org.scalatest"     %% "scalatest"            % scalatestVersion % Test,
  "org.typelevel"     %% "cats-core"            % catsVersion
)

val akkaJacksonProjectDependencies = Seq(
  "javax.annotation"               %  "javax.annotation-api"    % javaxAnnotationVersion, // for jdk11
  "javax.validation"               %  "validation-api"          % javaxValidationVersion,
  "javax.xml.bind"                 %  "jaxb-api"                % jaxbApiVersion, // for jdk11
  "com.typesafe.akka"              %% "akka-http"               % akkaHttpVersion,
  "com.typesafe.akka"              %% "akka-http-testkit"       % akkaHttpVersion,
  "com.typesafe.akka"              %% "akka-stream"             % akkaVersion,
  "com.typesafe.akka"              %% "akka-testkit"            % akkaVersion,
  "com.fasterxml.jackson.core"     %  "jackson-core"            % jacksonVersion,
  "com.fasterxml.jackson.core"     %  "jackson-databind"        % jacksonVersion,
  "com.fasterxml.jackson.core"     %  "jackson-annotations"     % jacksonVersion,
  "com.fasterxml.jackson.datatype" %  "jackson-datatype-jsr310" % jacksonVersion,
  "com.fasterxml.jackson.module"   %% "jackson-module-scala"    % jacksonVersion,
  "org.hibernate"                  %  "hibernate-validator"     % hibernateVersion,
  "org.glassfish"                  %  "javax.el"                % javaxElVersion,
  "org.typelevel"                  %% "cats-core"               % catsVersion,
  "org.scalatest"                  %% "scalatest"               % scalatestVersion % Test,
)

val http4sProjectDependencies = Seq(
  "javax.annotation" %  "javax.annotation-api"  % javaxAnnotationVersion, // for jdk11
  "javax.xml.bind"   % "jaxb-api"               % jaxbApiVersion, // for jdk11
  "io.circe"         %% "circe-core"            % circeVersion,
  "io.circe"         %% "circe-parser"          % circeVersion,
  "org.http4s"       %% "http4s-blaze-client"   % http4sVersion,
  "org.http4s"       %% "http4s-blaze-server"   % http4sVersion,
  "org.http4s"       %% "http4s-circe"          % http4sVersion,
  "org.http4s"       %% "http4s-dsl"            % http4sVersion,
  "org.scalatest"    %% "scalatest"             % scalatestVersion % Test,
  "org.typelevel"    %% "cats-core"             % catsVersion,
  "org.typelevel"    %% "cats-effect"           % catsEffectVersion
)

val dropwizardProjectDependencies = Seq(
  "javax.annotation"           %  "javax.annotation-api"   % javaxAnnotationVersion, // for jdk11
  "javax.xml.bind"             %  "jaxb-api"               % jaxbApiVersion, // for jdk11
  "io.dropwizard"              %  "dropwizard-core"        % dropwizardVersion,
  "io.dropwizard"              %  "dropwizard-forms"       % dropwizardVersion,
  "org.asynchttpclient"        %  "async-http-client"      % ahcVersion,
  "org.scala-lang.modules"     %% "scala-java8-compat"     % "0.9.1"            % Test,
  "org.scalatest"              %% "scalatest"              % scalatestVersion   % Test,
  "junit"                      %  "junit"                  % "4.13.1"             % Test,
  "nl.jqno.equalsverifier"     %  "equalsverifier"         % "3.5"            % Test,
  "com.novocode"               %  "junit-interface"        % "0.11"             % Test,
  "org.mockito"                %% "mockito-scala"          % "1.16.12"           % Test,
  "com.github.tomakehurst"     %  "wiremock"               % "2.27.2"           % Test,
  "io.dropwizard"              %  "dropwizard-testing"     % dropwizardVersion  % Test,
  "org.glassfish.jersey.test-framework.providers" % "jersey-test-framework-provider-grizzly2" % jerseyVersion % Test
)

val dropwizardScalaProjectDependencies = Seq(
  "javax.annotation"               %  "javax.annotation-api"    % javaxAnnotationVersion, // for jdk11
  "javax.xml.bind"                 %  "jaxb-api"                % jaxbApiVersion, // for jdk11
  "io.dropwizard"                  %  "dropwizard-core"         % dropwizardVersion,
  "io.dropwizard"                  %  "dropwizard-forms"        % dropwizardVersion,
  "com.datasift.dropwizard.scala"  %% "dropwizard-scala-core"   % dropwizardScalaVersion,
  "com.fasterxml.jackson.datatype" %  "jackson-datatype-jsr310" % jacksonVersion,
  "com.fasterxml.jackson.module"   %% "jackson-module-scala"    % jacksonVersion,
  "org.typelevel"                  %% "cats-core"               % catsVersion,
  "org.scala-lang.modules"         %% "scala-java8-compat"      % "0.9.1"            % Test,
  "org.scalatest"                  %% "scalatest"               % scalatestVersion   % Test,
  "junit"                          %  "junit"                   % "4.13.1"             % Test,
  "com.novocode"                   %  "junit-interface"         % "0.11"             % Test,
  "org.mockito"                    %% "mockito-scala-scalatest" % "1.16.12"           % Test,
  "com.github.tomakehurst"         %  "wiremock"                % "2.27.2"           % Test,
  "io.dropwizard"                  %  "dropwizard-testing"      % dropwizardVersion  % Test,
  "org.glassfish.jersey.test-framework.providers" % "jersey-test-framework-provider-grizzly2" % jerseyVersion % Test,
)

val dropwizardVavrProjectDependencies = dropwizardProjectDependencies ++ Seq(
  "io.vavr"               % "vavr"            % vavrVersion,
  "io.vavr"               % "vavr-jackson"    % vavrVersion,
  "io.dropwizard.modules" % "dropwizard-vavr" % dropwizardVavrVersion,
)

val springProjectDependencies = Seq(
  "org.springframework.boot"   %  "spring-boot-starter-web"  % springBootVersion,
  "javax.annotation"           %  "javax.annotation-api"    % javaxAnnotationVersion, // for jdk11
  "javax.validation"           %  "validation-api"           % javaxValidationVersion,
  "org.scala-lang.modules"     %% "scala-java8-compat"       % "0.9.1"            % Test,
  "org.scalatest"              %% "scalatest"                % scalatestVersion   % Test,
  "org.mockito"                %% "mockito-scala"            % "1.16.12"           % Test,
  "org.springframework.boot"   %  "spring-boot-starter-test" % springBootVersion  % Test,
)

def buildSampleProject(name: String, extraLibraryDependencies: Seq[sbt.librarymanagement.ModuleID]) =
  Project(s"${name}Sample", file(s"modules/sample-${name}"))
    .settings(commonSettings)
    .settings(codegenSettings)
    .settings(
      libraryDependencies ++= extraLibraryDependencies,
      unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated",
      skip in publish := true,
      scalafmtOnCompile := false
    )

lazy val akkaHttpSample = buildSampleProject("akkaHttp", akkaProjectDependencies)

lazy val akkaHttpJacksonSample = buildSampleProject("akkaHttpJackson", akkaJacksonProjectDependencies)

lazy val dropwizardScalaSample = buildSampleProject("dropwizardScala", dropwizardScalaProjectDependencies)

lazy val http4sSample = buildSampleProject("http4s", http4sProjectDependencies)

val javaSampleSettings = Seq(
    testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
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

lazy val endpointsDependencies = (project in file("modules/sample-endpoints-deps"))
  .settings(commonSettings)
  .settings(
    skip in publish := true
  )
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe"          %%% "circe-core"                    % circeVersion,
      "io.circe"          %%% "circe-parser"                  % circeVersion,
      "io.github.cquiroz" %%% "scala-java-time"               % "2.0.0",
      "org.julienrf"      %%% "endpoints-algebra"             % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client"          % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client-circe"    % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client-faithful" % endpointsVersion,
      "org.scalatest"     %%% "scalatest"                     % scalatestVersion % Test,
      "org.typelevel"     %%% "cats-core"                     % catsVersion
    ),
  )

lazy val endpointsSample = (project in file("modules/sample-endpoints"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings)
  .settings(
    coverageEnabled := false,  // scoverage issue @ commit 28b0cc55: Found a dangling UndefinedParam at Position(file:.../modules/sample-endpoints/target/generated/issues/issue351/client/endpoints/EndpointsImplicits.scala,91,34). This is likely due to a bad interaction between a macro or a compiler plugin and the Scala.js compiler plugin. If you hit this, please let us know.
    codegenSettings,
    libraryDependencies ++= Seq(
      "io.circe"          %%% "circe-core"                    % circeVersion,
      "io.circe"          %%% "circe-parser"                  % circeVersion,
      "io.github.cquiroz" %%% "scala-java-time"               % "2.0.0",
      "org.julienrf"      %%% "endpoints-algebra"             % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client"          % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client-circe"    % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client-faithful" % endpointsVersion,
      "org.scalatest"     %%% "scalatest"                     % scalatestVersion % Test,
      "org.typelevel"     %%% "cats-core"                     % catsVersion
    ),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated",
    skip in publish := true,
    scalafmtOnCompile := false
  )

lazy val microsite = (project in file("modules/microsite"))
  .settings(commonSettings)
  .settings(
    skip in publish := true
  )
  .dependsOn(codegen)

watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.scala").get
watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.java").get

logBuffered in Test := false
