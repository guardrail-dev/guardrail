package core.issues

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.terms.protocol.{ ClassDefinition, StaticDefns }

class Issue538 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  def testVersion(version: Http4sVersion): Unit = {
    test(s"$version - Test double inheritance - both optional") {
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
            _: ClassDefinition[ScalaLanguage], // bar
            _: ClassDefinition[ScalaLanguage], // baz
            foo: ClassDefinition[ScalaLanguage]
          ),
          _,
          _,
          _,
          _
        ),
        _,
        _
      ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      val companion = companionForStaticDefns(foo.staticDefns)
      cmp(foo.cls, q"case class Foo(id: Option[String] = None)")
    }

    test(s"$version - Test double inheritance - one required") {
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
            _: ClassDefinition[ScalaLanguage], // bar
            _: ClassDefinition[ScalaLanguage], // baz
            foo: ClassDefinition[ScalaLanguage]
          ),
          _,
          _,
          _,
          _
        ),
        _,
        _
      ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      cmp(foo.cls, q"case class Foo(id: String)")
    }

    test(s"$version - Test double inheritance - data redaction") {
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
            _: ClassDefinition[ScalaLanguage], // bar
            _: ClassDefinition[ScalaLanguage], // baz
            foo: ClassDefinition[ScalaLanguage]
          ),
          _,
          _,
          _,
          _
        ),
        _,
        _
      ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      cmp(foo.cls, q"""case class Foo(id: String) { override def toString: String = "Foo(" + "[redacted]" + ")" }""")
    }

    test(s"$version - Test double inheritance - empty to null") {
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
            _: ClassDefinition[ScalaLanguage], // bar
            _: ClassDefinition[ScalaLanguage], // baz
            foo: ClassDefinition[ScalaLanguage]
          ),
          _,
          _,
          _,
          _
        ),
        _,
        _
      ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      val companion = companionForStaticDefns(foo.staticDefns)
      val expected =
        q"""
        object Foo {
          implicit val encodeFoo: _root_.io.circe.Encoder.AsObject[Foo] = {
            _root_.io.circe.Encoder.AsObject.instance[Foo](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("id", a.id.asJson))))
          }
          implicit val decodeFoo: _root_.io.circe.Decoder[Foo] = new _root_.io.circe.Decoder[Foo] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Foo] = for (v0 <- c.downField("id").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) _root_.io.circe.Json.Null else j)).as[String]) yield Foo(v0) }
        }
       """
      cmp(companion, expected)
    }
  }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)

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
