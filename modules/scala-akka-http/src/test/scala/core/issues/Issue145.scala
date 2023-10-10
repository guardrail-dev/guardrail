package core.issues

import scala.meta._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail._
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.ClassDefinition

class Issue145 extends AnyFunSpec with Matchers with SwaggerSpecRunner {

  describe("Generate hierarchical classes") {

    val spec: String = """
      | spec: '2.0'
      | info:
      |   title: Parsing Error Sample
      |   version: 1.0.0
      | definitions:
      |   Pet:
      |     type: object
      |     properties:
      |       name:
      |         type: string
      |         x-scala-empty-is-null: true
      |         x-scala-type: CustomThing
      |       underscore_name:
      |         type: string
      |         x-scala-empty-is-null: true
      |         x-scala-type: CustomThing
      |       dash-name:
      |         type: string
      |         x-scala-empty-is-null: true
      |         x-scala-type: CustomThing
      |""".stripMargin

    val (
      ProtocolDefinitions(
        ClassDefinition(namePet, tpePet, _, clsPet, staticDefnsPet, catParents) :: Nil,
        _,
        _,
        _,
        _
      ),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    it("should generate right companion object") {
      val cmp = companionForStaticDefns(staticDefnsPet)
      val companion = q"""
        object Pet {
          implicit val encodePet: _root_.io.circe.Encoder.AsObject[Pet] = {
            _root_.io.circe.Encoder.AsObject.instance[Pet](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("name", a.name.asJson), ("underscore_name", a.underscoreName.asJson), ("dash-name", a.dashName.asJson))))
          }
          implicit val decodePet: _root_.io.circe.Decoder[Pet] = new _root_.io.circe.Decoder[Pet] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Pet] = for (v0 <- c.downField("name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) _root_.io.circe.Json.Null else j)).as[Option[CustomThing]]; v1 <- c.downField("underscore_name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) _root_.io.circe.Json.Null else j)).as[Option[CustomThing]]; v2 <- c.downField("dash-name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) _root_.io.circe.Json.Null else j)).as[Option[CustomThing]]) yield Pet(v0, v1, v2) }
        }
      """

      cmp.structure shouldBe companion.structure
    }

  }
}
