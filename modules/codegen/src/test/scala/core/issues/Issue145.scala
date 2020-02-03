package core.issues

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import org.scalatest.{ FunSpec, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue145 extends FunSpec with Matchers with SwaggerSpecRunner {

  describe("Generate hierarchical classes") {

    val swagger: String = """
      | swagger: '2.0'
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
        _
      ),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    it("should generate right companion object") {
      val cmp       = companionForStaticDefns(staticDefnsPet)
      val companion = q"""
        object Pet {
          implicit val encodePet: Encoder.AsObject[Pet] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[Pet](a => JsonObject.fromIterable(Vector(("name", a.name.asJson), ("underscore_name", a.underscoreName.asJson), ("dash-name", a.dashName.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodePet: Decoder[Pet] = new Decoder[Pet] { final def apply(c: HCursor): Decoder.Result[Pet] = for (v0 <- c.downField("name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[Option[CustomThing]]; v1 <- c.downField("underscore_name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[Option[CustomThing]]; v2 <- c.downField("dash-name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[Option[CustomThing]]) yield Pet(v0, v1, v2) }
        }
      """

      cmp.structure shouldBe companion.structure
    }

  }
}
