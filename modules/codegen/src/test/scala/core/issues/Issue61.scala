package tests.core.issues

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Context, ProtocolDefinitions, RandomType }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue61 extends FunSuite with Matchers with SwaggerSpecRunner {
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
    |    type: array
    |    items:
    |      type: string
    |  Bar:
    |    type: integer
    |    format: int64
    |""".stripMargin

  test("Generate plain array alias definition") {
    val (
      ProtocolDefinitions(RandomType(_, tpe) :: _ :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    tpe.structure shouldBe t"Vector[String]".structure
  }

  test("Generate primitive type aliases") {
    val (
      ProtocolDefinitions(_ :: RandomType(_, tpe) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    tpe.structure shouldBe t"Long".structure
  }
}
