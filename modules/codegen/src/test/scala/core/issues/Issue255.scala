package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions, RandomType }
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
                           |""".stripMargin

  test("Test password format generation") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, c1, _, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    c1.structure shouldBe q"case class Foo(someEmail: Option[String] = None, somePassword: Option[String] = None, someFile: Option[java.io.File] = None, someBinary: Option[java.io.File] = None)".structure
  }
}
