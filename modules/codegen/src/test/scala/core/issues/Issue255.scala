package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue255 extends FunSuite with Matchers with SwaggerSpecRunner {
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
                           |      someEmail:
                           |        type: string
                           |        format: email
                           |      somePassword:
                           |        type: string
                           |        format: password
                           |      someFile:
                           |        type: file
                           |      someBinary:
                           |        type: string
                           |        format: binary
                           |      someCustomBinary:
                           |        type: file
                           |        x-scala-type: custom.Bytes
                           |""".stripMargin

  test("Test password format generation") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, c1, _, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val expected =
      q"""
        case class Foo(someEmail: scala.Option[String] = scala.None,
                       somePassword: scala.Option[String] = scala.None,
                       someFile: scala.Option[String] = scala.None,
                       someBinary: scala.Option[String] = scala.None,
                       someCustomBinary: scala.Option[custom.Bytes] = scala.None)
       """
    c1.structure shouldBe expected.structure
  }
}
