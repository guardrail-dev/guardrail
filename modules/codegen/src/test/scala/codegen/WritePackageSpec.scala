package codegen

import java.nio.file.{ Path, Paths }

import _root_.io.swagger.models.Swagger
import _root_.io.swagger.parser.SwaggerParser
import cats.data.NonEmptyList
import com.twilio.guardrail._
import com.twilio.guardrail.core.CoreTermInterp
import com.twilio.guardrail.terms.CoreTerm
import org.scalatest.{ FunSuite, Matchers }
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._

class WritePackageSpec extends FunSuite with Matchers {
  val swagger: Swagger = new SwaggerParser().parse(s"""
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
    |      x-scala-package: foo
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
    |    x-scala-package: com.example.test
    |    properties:
    |      a:
    |        type: string
    |""".stripMargin)

  def injectSwagger[T](s: Swagger, rs: ReadSwagger[T]): T = rs.next(s)

  def extractPackage(path: Path, results: List[WriteTree]): Term.Ref = {
    val Some(source"""package ${fooPkg }
    ..${stats }
    """) = results.find(_.path == path).headOption.map(_.contents)
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
    val generatorSettings = new GeneratorSettings(t"BodyPartEntity", t"io.circe.Json")

    val result: List[WriteTree] = CoreTarget
      .unsafeExtract(Common.processArgs[CoreTerm](args).foldMap(CoreTermInterp))
      .toList
      .flatMap({ case (_, x) => Target.unsafeExtract(injectSwagger(swagger, x), generatorSettings) })

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

    val allPaths = Set(fooPath, barPath, pkgPath, clientPath, implicitsPath, frameworkImplicitsPath)

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
    val generatorSettings = new GeneratorSettings(t"BodyPartEntity", t"io.circe.Json")

    val result: List[WriteTree] = CoreTarget
      .unsafeExtract(Common.processArgs[CoreTerm](args).foldMap(CoreTermInterp))
      .toList
      .flatMap({ case (_, x) => Target.unsafeExtract(injectSwagger(swagger, x), generatorSettings) })

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

    val allPaths = Set(fooPath, barPath, pkgPath, clientPath, implicitsPath, frameworkImplicitsPath)

    result.filterNot(wf => allPaths.contains(wf.path)).length shouldBe 0
  }
}
