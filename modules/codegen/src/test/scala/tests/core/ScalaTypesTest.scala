package tests.core

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.ClassDefinition

class ScalaTypesTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  val swagger: String = s"""
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
    |        type: string
    |        x-scala-type: dev.guardrail.foo.bar.Baz
    |""".stripMargin

  test("Generate no definitions") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Baz(foo: Option[dev.guardrail.foo.bar.Baz] = None)
    """

    val companion = q"""
      object Baz {
        implicit val encodeBaz: _root_.io.circe.Encoder.AsObject[Baz] = {
          val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String]()
          _root_.io.circe.Encoder.AsObject.instance[Baz](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("foo", a.foo.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeBaz: _root_.io.circe.Decoder[Baz] = new _root_.io.circe.Decoder[Baz] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Baz] = for (v0 <- c.downField("foo").as[Option[dev.guardrail.foo.bar.Baz]]) yield Baz(v0) }
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }
}
