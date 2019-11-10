package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue223 extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
                           |swagger: "2.0"
                           |info:
                           |  title: Whatever
                           |  version: 1.0.0
                           |host: localhost:1234
                           |schemes:
                           |  - http
                           |definitions:
                           |  Kernel:
                           |    description: Kernel information
                           |    required:
                           |     - id
                           |    properties:
                           |      id:
                           |        type: string
                           |        format: uuid
                           |        description: uuid of kernel
                           |""".stripMargin

  test("Test uuid format generation") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, c1, _, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    c1.structure shouldBe q"case class Kernel(id: java.util.UUID)".structure
  }
}
