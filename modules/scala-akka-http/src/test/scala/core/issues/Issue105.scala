package tests.core.issues

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.ClassDefinition

class Issue105 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  Foo:
    |    type: object
    |    properties:
    |      nonEmptyString:
    |        type: string
    |        format: int64
    |        x-scala-type: String Refined NonEmpty
    |      positiveLong:
    |        type: integer
    |        format: int64
    |        x-scala-type: Long Refined Positive
    |""".stripMargin

  test("Generate plain array alias definition") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Foo(nonEmptyString: Option[String Refined NonEmpty] = None, positiveLong: Option[Long Refined Positive] = None)
    """

    val companion = q"""
      object Foo {
        implicit val encodeFoo: _root_.io.circe.Encoder.AsObject[Foo] = {
          _root_.io.circe.Encoder.AsObject.instance[Foo](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("nonEmptyString", a.nonEmptyString.asJson), ("positiveLong", a.positiveLong.asJson))))
        }
        implicit val decodeFoo: _root_.io.circe.Decoder[Foo] = new _root_.io.circe.Decoder[Foo] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Foo] = for (v0 <- c.downField("nonEmptyString").as[Option[String Refined NonEmpty]]; v1 <- c.downField("positiveLong").as[Option[Long Refined Positive]]) yield Foo(v0, v1) }
      }
    """

    cls.structure shouldBe definition.structure
    cmp.structure shouldBe companion.structure
  }
}
