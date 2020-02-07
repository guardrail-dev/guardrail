package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import com.twilio.guardrail.languages.ScalaLanguage
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

class Issue538 extends FunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  test("Test double inheritance - both optional") {
    val swagger: String = """
                            |swagger: '2.0'
                            |definitions:
                            |  Bar:
                            |    properties:
                            |      id:
                            |        type: string
                            |  Baz:
                            |    properties:
                            |      id:
                            |        type: string
                            |  Foo:
                            |    allOf:
                            |      - $ref: "#/definitions/Bar"
                            |      - $ref: "#/definitions/Baz"
                            |""".stripMargin

    val (
      ProtocolDefinitions(
        List(
          _: ClassDefinition[ScalaLanguage], //bar
          _: ClassDefinition[ScalaLanguage], //baz
          foo: ClassDefinition[ScalaLanguage]
        ),
        _,
        _,
        _
      ),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    cmp(foo.cls, q"case class Foo(id: Option[String] = None)")
  }

  test("Test double inheritance - one required") {
    val swagger: String = """
                            |swagger: '2.0'
                            |definitions:
                            |  Bar:
                            |    properties:
                            |      id:
                            |        type: string
                            |    required: [id]
                            |  Baz:
                            |    properties:
                            |      id:
                            |        type: string
                            |  Foo:
                            |    allOf:
                            |      - $ref: "#/definitions/Bar"
                            |      - $ref: "#/definitions/Baz"
                            |""".stripMargin

    val (
      ProtocolDefinitions(
        List(
          _: ClassDefinition[ScalaLanguage], //bar
          _: ClassDefinition[ScalaLanguage], //baz
          foo: ClassDefinition[ScalaLanguage]
        ),
        _,
        _,
        _
      ),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    cmp(foo.cls, q"case class Foo(id: String)")
  }

  private def cmp(l: Tree, r: Tree): Unit =
    l.structure shouldBe r.structure

}
