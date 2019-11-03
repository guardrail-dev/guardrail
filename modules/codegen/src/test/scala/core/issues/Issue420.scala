package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

class Issue420 extends FunSuite with Matchers with SwaggerSpecRunner {

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
      ProtocolDefinitions(List(bar: ClassDefinition[ScalaLanguage], foo: ClassDefinition[ScalaLanguage]), _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    cmp(bar.cls, q"case class Bar(id: Option[String] = None)")
    cmp(foo.cls, q"case class Foo(id: Option[String] = None, otherId: Option[String] = None)")
  }

  private def cmp(l: Tree, r: Tree): Unit =
    l.structure shouldBe r.structure

}
