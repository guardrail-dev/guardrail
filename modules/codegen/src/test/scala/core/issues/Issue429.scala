package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue429 extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |openapi: 3.0.1
    |components:
    |  schemas:
    |    Answer:
    |      type: object
    |      properties:
    |        STATUS_CODE:
    |          type: string
    |          enum: [0, 1]
    |""".stripMargin

  test("Test correct escaping of numbers used as identifiers") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, _, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val List(_, _, _, statusCodeCompanion) = staticDefns.definitions

    val expected = q"""
      object StatusCode {
        object members {
          case object `0` extends StatusCode("0")
          case object `1` extends StatusCode("1")
        }
        val `0`: StatusCode = members.`0`
        val `1`: StatusCode = members.`1`
        val values = Vector(`0`, `1`)
        implicit val encodeStatusCode: Encoder[StatusCode] = Encoder[String].contramap(_.value)
        implicit val decodeStatusCode: Decoder[StatusCode] = Decoder[String].emap(value => parse(value).toRight(s"$$value not a member of StatusCode"))
        implicit val addPathStatusCode: AddPath[StatusCode] = AddPath.build(_.value)
        implicit val showStatusCode: Show[StatusCode] = Show.build(_.value)
        def parse(value: String): Option[StatusCode] = values.find(_.value == value)
        implicit val order: cats.Order[StatusCode] = cats.Order.by[StatusCode, Int](values.indexOf)
      }
      """

    statusCodeCompanion.structure shouldBe expected.structure
    Term.Name("10").syntax shouldBe "`10`"
  }
}
