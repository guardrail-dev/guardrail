package dev.guardrail.cli

import java.util.concurrent.atomic.AtomicReference
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import support.SwaggerSpecRunner

class CLICommonSpec extends AnyFunSuite with SwaggerSpecRunner with Matchers with OptionValues {
  def parseArgs(args: Array[String]): Vector[String] =
    // Hard-coded to scala because cli.dependsOn(scalaSupport % "test->compile")
    parseArgsLang("scala", args)
  def parseArgsLang(language: String, args: Array[String]): Vector[String] = {
    val state = new AtomicReference(Vector.empty[String])
    val impl = new CLICommon {
      val AnsiColor = new {
        val BLUE: String  = ""
        val BOLD: String  = ""
        val CYAN: String  = ""
        val RED: String   = ""
        val RESET: String = ""
        val WHITE: String = ""
      }

      def putErrLn(string: String): Unit = state.updateAndGet(_ :+ string)
    }
    impl.run(language, args)
    state.get()
  }

  test("Render --help") {
    parseArgs(
      Array("--help")
    ).headOption.value should include("Start specifying arguments for a new client")

    parseArgs(
      Array(
        "--client",
        "--specPath",
        "foo.yaml",
        "--outputPath",
        "bar",
        "--packageName",
        "baz",
        "--module",
        "akka-http",
        "--module",
        "circe",
        "--module",
        "foo",
        "--help"
      )
    ).headOption.value should include("Start specifying arguments for a new client")
  }

  test("Missing required args") {
    parseArgs(
      Array("--client", "--outputPath", "bar", "--packageName", "baz")
    ).headOption.value should startWith("Missing argument: --specPath")

    parseArgs(
      Array("--client", "--specPath", "foo", "--packageName", "baz")
    ).headOption.value should startWith("Missing argument: --outputPath")

    parseArgs(
      Array("--client", "--specPath", "foo", "--outputPath", "bar")
    ).headOption.value should startWith("Missing argument: --packageName")
  }

  test("Indicate which module is missing for an unsupported language") {
    parseArgsLang("foo", Array("--client")).headOption.value should startWith("Missing dependency: foo-support")
  }

  test("Correctly log unexpected arguments") {
    parseArgs(Array("--client", "foo", "bar", "baz")).headOption.value should be("Unknown arguments: foo bar baz")
  }

  test("Raise unexpected framework") {
    parseArgs(Array("--client", "--specPath", "foo.yaml", "--outputPath", "bar", "--packageName", "baz", "--framework", "foo")).headOption.value should be(
      "Unknown framework specified: foo"
    )
  }

  test("Unspecified modules") {
    val Vector(header, client, framework, protocol, server, "") =
      parseArgs(Array("--client", "--specPath", "foo.yaml", "--outputPath", "bar", "--packageName", "baz", "--module", "foo"))

    header should be("Unsatisfied module(s):")
    client should include("ClientGenerator:")
    client should include("akka-http")
    framework should include("akka-http")
    protocol should include("circe")
    server should include("akka-http")
  }

  test("Flag extraneous --modules") {
    parseArgs(
      Array(
        "--client",
        "--specPath",
        "foo.yaml",
        "--outputPath",
        "bar",
        "--packageName",
        "baz",
        "--module",
        "akka-http",
        "--module",
        "circe",
        "--module",
        "foo"
      )
    ).headOption.value should be("Unused modules specified: foo")
  }

  test("Raise missing modules") {
    val Vector(header, protocol, "") = parseArgs(
      Array(
        "--client",
        "--specPath",
        "foo.yaml",
        "--outputPath",
        "bar",
        "--packageName",
        "baz",
        "--module",
        "akka-http"
      )
    )
    header should be("Unsatisfied module(s):")
    protocol should include("circe")
  }

  test("Raise module conflict") {
    parseArgs(
      Array(
        "--client",
        "--specPath",
        "foo.yaml",
        "--outputPath",
        "bar",
        "--packageName",
        "baz",
        "--module",
        "akka-http",
        "--module",
        "http4s",
        "--module",
        "circe"
      )
    ).headOption.value should include("Too many modules specified for ClientGenerator")
  }
}
