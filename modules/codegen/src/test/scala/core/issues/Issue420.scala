package core.issues

import dev.guardrail.generators.Scala.Http4s
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue420 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String = """
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

  test("Test ordering") {
    val (
      ProtocolDefinitions(List(bar: ClassDefinition[ScalaLanguage], foo: ClassDefinition[ScalaLanguage]), _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    cmp(bar.cls, q"case class Bar(id: Option[String] = None)")
    cmp(foo.cls, q"case class Foo(id: Option[String] = None, otherId: Option[String] = None)")
  }

  private def cmp(l: Tree, r: Tree): Unit =
    l.structure shouldBe r.structure

}
