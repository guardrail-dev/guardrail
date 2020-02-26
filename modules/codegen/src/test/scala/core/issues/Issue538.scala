package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions, StaticDefns }
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

    val companion = companionForStaticDefns(foo.staticDefns)
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

  test("Test double inheritance - data redaction") {
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
                            |        x-data-redaction: true
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

    cmp(foo.cls, q"""case class Foo(id: String) { override def toString: String = "Foo(" + "[redacted]" + ")" }""")
  }

  test("Test double inheritance - empty to null") {
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
                            |        x-empty-is-null: true
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

    val companion = companionForStaticDefns(foo.staticDefns)
    val expected =
      q"""
        object Foo {
          implicit val encodeFoo: Encoder.AsObject[Foo] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[Foo](a => JsonObject.fromIterable(Vector(("id", a.id.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeFoo: Decoder[Foo] = new Decoder[Foo] { final def apply(c: HCursor): Decoder.Result[Foo] = for (v0 <- c.downField("id").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[String]) yield Foo(v0) }
        }
       """
    cmp(companion, expected)
  }

  private def companionForStaticDefns(staticDefns: StaticDefns[ScalaLanguage]): Defn.Object =
    q"""
    object ${Term.Name(staticDefns.className)} {
      ..${staticDefns.extraImports}
      ..${staticDefns.definitions}
    }
    """

  private def cmp(l: Tree, r: Tree): Unit =
    l.structure shouldBe r.structure

}
