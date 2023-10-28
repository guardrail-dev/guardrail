package core.issues

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import support.SwaggerSpecRunner
import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.ClassDefinition
import org.scalatest.Inside
import org.scalatest.Inspectors.forAll

class ScalaTypesTest extends AnyFunSuite with Matchers with SwaggerSpecRunner with Inside {

  val spec20: String =
    s"""
       |swagger: "2.0"
       |info:
       |  title: Whatever
       |  version: 1.0.0
       |host: localhost:1234
       |definitions:
       |  Baz:
       |    type: object
       |    properties:
       |      foo:
       |        $$ref: '#/definitions/Baz'
       |""".stripMargin

  val spec30: String =
    s"""
       |openapi: "3.0.0"
       |info:
       |  title: Whatever
       |  version: 1.0.0
       |host: localhost:1234
       |components:
       |  schemas:
       |    Baz:
       |      type: object
       |      properties:
       |        foo:
       |          $$ref: '#/components/schemas/Baz'
       |""".stripMargin

  val spec31: String =
    s"""
       |openapi: "3.1.0"
       |info:
       |  title: Whatever
       |  version: 1.0.0
       |host: localhost:1234
       |components:
       |  schemas:
       |    Baz:
       |      type: object
       |      properties:
       |        foo:
       |          $$ref: '#/components/schemas/Baz'
       |""".stripMargin

  test("Generate no definitions") {
    forAll(Seq(spec20, spec30, spec31)) { spec =>
      val result = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")
      val (ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _), _, _) = result

      val cmp = companionForStaticDefns(staticDefns)

      val definition =
        q"""
       case class Baz(foo: Option[Baz] = None)
     """

      val companion =
        q"""
       object Baz {
         implicit val encodeBaz: _root_.io.circe.Encoder.AsObject[Baz] = {
           _root_.io.circe.Encoder.AsObject.instance[Baz](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("foo", a.foo.asJson))))
         }
         implicit val decodeBaz: _root_.io.circe.Decoder[Baz] = new _root_.io.circe.Decoder[Baz] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Baz] = for (v0 <- c.downField("foo").as[Option[Baz]]) yield Baz(v0) }
       }
     """

      cls.syntax shouldEqual definition.syntax
      cmp.syntax shouldEqual companion.syntax
    }
  }
}
