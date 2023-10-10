package core.issues

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.terms.protocol.ClassDefinition

class Issue420 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val spec: String = """
                          |swagger: '2.0'
                          |definitions:
                          |  Bar:
                          |    properties:
                          |      id:
                          |        type: string
                          |  Foo:
                          |    allOf:
                          |      - type: object
                          |        properties:
                          |          otherId:
                          |            type: string
                          |      - $ref: "#/definitions/Bar"
                       |""".stripMargin

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Test ordering") {
      val (
        ProtocolDefinitions(List(bar: ClassDefinition[ScalaLanguage], foo: ClassDefinition[ScalaLanguage]), _, _, _, _),
        _,
        _
      ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, version.value)

      cmp(bar.cls, q"case class Bar(id: Option[String] = None)")
      cmp(foo.cls, q"case class Foo(id: Option[String] = None, otherId: Option[String] = None)")
    }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)

  private def cmp(l: Tree, r: Tree): Unit =
    l.structure shouldBe r.structure

}
