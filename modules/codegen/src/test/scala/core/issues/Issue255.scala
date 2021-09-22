package core.issues

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.http4s.Http4s
import dev.guardrail.terms.protocol.ClassDefinition

class Issue255 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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
      ProtocolDefinitions(ClassDefinition(_, _, _, c1, _, _) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val expected =
      q"""
        case class Foo(someEmail: Option[String] = None,
                       somePassword: Option[String] = None,
                       someFile: Option[String] = None,
                       someBinary: Option[String] = None,
                       someCustomBinary: Option[custom.Bytes] = None)
       """
    c1.structure shouldBe expected.structure
  }
}
