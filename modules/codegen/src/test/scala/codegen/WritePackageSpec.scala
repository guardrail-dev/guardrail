package codegen

import java.nio.file.{ Path, Paths }
import java.util

import _root_.io.swagger.v3.oas.models.OpenAPI
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail._
import dev.guardrail.core.CoreTermInterp
import dev.guardrail.languages.ScalaLanguage

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions

import scala.meta._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class WritePackageSpec extends AnyFunSuite with Matchers {
  val parseOpts = new ParseOptions
  parseOpts.setResolve(true)
  val swagger: OpenAPI = new OpenAPIParser()
    .readContents(
      s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /foo:
    |    get:
    |      operationId: getFoo
    |      x-jvm-package: foo
    |      parameters:
    |      - in: body
    |        name: foo
    |        schema:
    |          $$ref: "#/definitions/Foo"
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            $$ref: "#/definitions/Bar"
    |definitions:
    |  Foo:
    |    type: object
    |    properties:
    |      a:
    |        type: string
    |  Bar:
    |    type: object
    |    x-jvm-package: com.example.test
    |    properties:
    |      a:
    |        type: string
    |""".stripMargin,
      new util.LinkedList(),
      parseOpts
    )
    .getOpenAPI

  def injectSwagger[T](s: OpenAPI, rs: ReadSwagger[T]): T = rs.next(s)

  def extractPackage(path: Path, results: List[WriteTree]): Term.Ref = {
    val Some(source"""package ${fooPkg }
    ..${stats }
    """) = results.find(_.path == path).headOption.map(_.contents).map(x => new String(Target.unsafeExtract(Await.result(x, Duration.Inf))).parse[Source].get)
    fooPkg
  }

  test("CLI should provide sane defaults for paths") {
    val args = NonEmptyList(
      Args.empty.copy(
        kind = CodegenTarget.Client,
        specPath = Some("/tmp/foo.json"),
        outputPath = Some("/tmp/foo"),
        packageName = Some(List("com", "twilio", "example", "clients")),
        context = Context.empty.copy(framework = Some("akka-http"))
      ),
      List.empty
    )

    import dev.guardrail.generators.ScalaModule
    import dev.guardrail.generators.Scala.AkkaHttp
    val result: List[WriteTree] = Target
      .unsafeExtract(
        Common
          .processArgs[ScalaLanguage, Target](args)(
            new CoreTermInterp[ScalaLanguage](
              "akka-http",
              ScalaModule.extract, {
                case "akka-http" => AkkaHttp
              }, {
                _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
              }
            )
          )
      )
      .toList
      .flatMap(x => Target.unsafeExtract(injectSwagger(swagger, x)))

    val paths = result.map(_.path)

    val fooPath =
      Paths.get("/tmp/foo/com/twilio/example/clients/definitions/Foo.scala")
    paths should contain(fooPath)

    val barPath =
      Paths.get("/tmp/foo/com/twilio/example/clients/definitions/Bar.scala")
    paths should contain(barPath)

    val pkgPath =
      Paths.get("/tmp/foo/com/twilio/example/clients/definitions/package.scala")
    paths should contain(pkgPath)

    val clientPath =
      Paths.get("/tmp/foo/com/twilio/example/clients/foo/FooClient.scala")
    paths should contain(clientPath)

    val implicitsPath =
      Paths.get("/tmp/foo/com/twilio/example/clients/Implicits.scala")
    paths should contain(implicitsPath)

    val frameworkImplicitsPath =
      Paths.get("/tmp/foo/com/twilio/example/clients/AkkaHttpImplicits.scala")
    paths should contain(frameworkImplicitsPath)

    val presencePath = Paths.get("/tmp/foo/com/twilio/example/clients/support/Presence.scala")
    paths should contain(presencePath)

    val allPaths = Set(fooPath, barPath, pkgPath, clientPath, implicitsPath, frameworkImplicitsPath, presencePath)

    result.filterNot(wf => allPaths.contains(wf.path)).length shouldBe 0
  }

  test("CLI properly generates all WriteTrees") {
    val args = NonEmptyList(
      Args.empty.copy(
        kind = CodegenTarget.Client,
        specPath = Some("/tmp/foo.json"),
        outputPath = Some("/tmp/foo"),
        packageName = Some(List("base")),
        context = Context.empty.copy(framework = Some("akka-http"))
      ),
      List.empty
    )

    import dev.guardrail.generators.ScalaModule
    import dev.guardrail.generators.Scala.AkkaHttp
    val result: List[WriteTree] = Target
      .unsafeExtract(
        Common
          .processArgs[ScalaLanguage, Target](args)(
            new CoreTermInterp[ScalaLanguage](
              "akka-http",
              ScalaModule.extract, {
                case "akka-http" => AkkaHttp
              }, {
                _.parse[Importer].toEither.bimap(err => UnparseableArgument("import", err.toString), importer => Import(List(importer)))
              }
            )
          )
      )
      .toList
      .flatMap(x => Target.unsafeExtract(injectSwagger(swagger, x)))

    val paths = result.map(_.path)

    val fooPath = Paths.get("/tmp/foo/base/definitions/Foo.scala")
    paths should contain(fooPath)
    extractPackage(fooPath, result).structure shouldBe q"base.definitions".structure

    val barPath = Paths.get("/tmp/foo/base/definitions/Bar.scala")
    paths should contain(barPath)
    extractPackage(barPath, result).structure shouldBe q"base.definitions".structure

    val pkgPath = Paths.get("/tmp/foo/base/definitions/package.scala")
    paths should contain(pkgPath)
    extractPackage(pkgPath, result).structure shouldBe q"base".structure

    val clientPath = Paths.get("/tmp/foo/base/foo/FooClient.scala")
    paths should contain(clientPath)
    extractPackage(clientPath, result).structure shouldBe q"base.foo".structure

    val implicitsPath = Paths.get("/tmp/foo/base/Implicits.scala")
    paths should contain(implicitsPath)
    extractPackage(implicitsPath, result).structure shouldBe q"base".structure

    val frameworkImplicitsPath =
      Paths.get("/tmp/foo/base/AkkaHttpImplicits.scala")
    paths should contain(frameworkImplicitsPath)
    extractPackage(frameworkImplicitsPath, result).structure shouldBe q"base".structure

    val presencePath = Paths.get("/tmp/foo/base/support/Presence.scala")
    paths should contain(presencePath)
    extractPackage(presencePath, result).structure shouldBe q"base.support".structure

    val allPaths = Set(fooPath, barPath, pkgPath, clientPath, implicitsPath, frameworkImplicitsPath, presencePath)

    result.filterNot(wf => allPaths.contains(wf.path)).length shouldBe 0
  }
}
