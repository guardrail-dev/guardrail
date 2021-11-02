package core.issues

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.http4s.Http4s
import dev.guardrail.terms.protocol.ClassDefinition

class Issue223 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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
      ProtocolDefinitions(ClassDefinition(_, _, _, c1, _, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    c1.structure shouldBe q"case class Kernel(id: java.util.UUID)".structure
  }
}
