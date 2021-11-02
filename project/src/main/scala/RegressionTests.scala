package dev.guardrail.sbt

import sbt.file

object RegressionTests {
  val exampleFrameworkSuites = Map(
    "scala" -> List(
      ExampleFramework("akka-http", "akkaHttp"),
      ExampleFramework("endpoints", "endpoints", List()),
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

  def sampleResource(name: String): java.io.File = file(s"modules/sample/src/main/resources/${name}")
  val exampleCases: List[ExampleCase] = List(
    ExampleCase(sampleResource("department.yaml"), "department"),
    ExampleCase(sampleResource("additional-properties.yaml"), "additionalProperties"),
    ExampleCase(sampleResource("alias.yaml"), "alias"),
    ExampleCase(sampleResource("char-encoding/char-encoding-request-stream.yaml"), "charEncoding.requestStream").frameworks("java" -> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("dropwizard")),
    ExampleCase(sampleResource("char-encoding/char-encoding-response-stream.yaml"), "charEncoding.responseStream").frameworks("java"-> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("dropwizard")),
    ExampleCase(sampleResource("contentType-textPlain.yaml"), "tests.contentTypes.textPlain"),
    ExampleCase(sampleResource("custom-header-type.yaml"), "tests.customTypes.customHeader"),
    ExampleCase(sampleResource("date-time.yaml"), "dateTime"),
    ExampleCase(sampleResource("edgecases/defaults.yaml"), "edgecases.defaults"),
    ExampleCase(sampleResource("empty-is-null.yaml"), "emptyIsNull"),
    ExampleCase(sampleResource("invalid-characters.yaml"), "invalidCharacters").frameworks("java" -> Set("dropwizard", "dropwizard-vavr")),
    ExampleCase(sampleResource("formData.yaml"), "form"),
    ExampleCase(sampleResource("enumerations.yaml"), "enumerations"),
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
    ExampleCase(sampleResource("issues/issue572.yaml"), "issues.issue572"),
    ExampleCase(sampleResource("issues/issue622.yaml"), "issues.issue622"),
    ExampleCase(sampleResource("issues/issue1138.yaml"), "issues.issue1138"),
    ExampleCase(sampleResource("issues/issue1260.yaml"), "issues.issue1260"),
    ExampleCase(sampleResource("issues/issue1218.yaml"), "issues.issue1218").frameworks("scala" -> Set("http4s")),
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
    ExampleCase(sampleResource("pathological-parameters.yaml"), "pathological").frameworks("java" -> javaFrameworks.toSet, "scala" -> (scalaFrameworks.toSet - "endpoints")), // Blocked by https://github.com/endpoints4s/endpoints4s/issues/713
    ExampleCase(sampleResource("response-headers.yaml"), "responseHeaders"),
    ExampleCase(sampleResource("random-content-types.yaml"), "randomContentTypes").frameworks("java" -> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("http4s", "dropwizard")),
    ExampleCase(sampleResource("binary.yaml"), "binary").frameworks("java" -> Set("dropwizard", "dropwizard-vavr"), "scala" -> Set("http4s")),
    ExampleCase(sampleResource("conflicting-names.yaml"), "conflictingNames"),
    ExampleCase(sampleResource("base64.yaml"), "base64").frameworks("scala" -> scalaFrameworks.toSet),
    ExampleCase(sampleResource("server1.yaml"), "customExtraction").args("--custom-extraction").frameworks("scala" -> Set("akka-http", "http4s")),
    ExampleCase(sampleResource("mixed-content-types-3.0.2.yaml"), "mixedContentTypes").frameworks("scala" -> scalaFrameworks.toSet),
    ExampleCase(sampleResource("debug-body.yaml"), "debugBody").frameworks("scala" -> Set("http4s")),
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
}
