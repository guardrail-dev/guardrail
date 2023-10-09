package core.issues

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.terms.protocol.ClassDefinition

class Issue429 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val spec: String = s"""
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

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Test correct escaping of numbers used as identifiers") {
      val (
        ProtocolDefinitions(ClassDefinition(_, _, _, _, staticDefns, _) :: Nil, _, _, _, _),
        _,
        _
      ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, version.value)

      val List(_, _, _, statusCodeCompanion) = staticDefns.definitions

      val expected = q"""
      object StatusCode {
        object members {
          case object `0` extends StatusCode("0")
          case object `1` extends StatusCode("1")
        }
        val `0`: StatusCode = members.`0`
        val `1`: StatusCode = members.`1`
        val values = _root_.scala.Vector(`0`, `1`)
        implicit val encodeStatusCode: _root_.io.circe.Encoder[StatusCode] = _root_.io.circe.Encoder[String].contramap(_.value)
        implicit val decodeStatusCode: _root_.io.circe.Decoder[StatusCode] = _root_.io.circe.Decoder[String].emap(value => from(value).toRight(s"$$value not a member of StatusCode"))
        implicit val showStatusCode: Show[StatusCode] = Show[String].contramap[StatusCode](_.value)
        def from(value: String): _root_.scala.Option[StatusCode] = values.find(_.value == value)
        implicit val order: cats.Order[StatusCode] = cats.Order.by[StatusCode, Int](values.indexOf)
      }
      """

      statusCodeCompanion.structure shouldBe expected.structure
      Term.Name("10").syntax shouldBe "`10`"
    }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
