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
      val companionPet = companionForStaticDefns(staticDefnsPet)
      companionPet.toString() shouldBe q"""
        object Pet {
          implicit val encodePet: ObjectEncoder[Pet] = {
            val readOnlyKeys = Set[String]()
            Encoder.forProduct3("name", "underscore_name", "dash-name")((o: Pet) => (o.name, o.underscoreName, o.dashName)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodePet: Decoder[Pet] = new Decoder[Pet] {
            final def apply(c: HCursor): Decoder.Result[Pet] =
              for (
                name <- c.downField("name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[Option[CustomThing]];
                underscoreName <- c.downField("underscore_name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[Option[CustomThing]];
                dashName <- c.downField("dash-name").withFocus(j => j.asString.fold(j)(s => if (s.isEmpty) Json.Null else j)).as[Option[CustomThing]]
              ) yield Pet(name, underscoreName, dashName)
          }
        }""".toString()
    }

  }
}
