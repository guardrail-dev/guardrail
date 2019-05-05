package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ClassDefinition, Context, ProtocolDefinitions, RandomType}
import org.scalatest.{FunSuite, Matchers}
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
                           |      somePassword:
                           |        type: string
                           |        format: password
                           |      someFile:
                           |        type: file
                           |      someBinary:
                           |        type: string
                           |        name: binary
                           |""".stripMargin

  test("Test password format generation") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, c1, _, _) :: Nil, _, _, _),
      _,
      _
      ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val expected = q"case class Foo(somePassword: Option[String] = None, someFile: Option[java.io.File] = None, someBinary: Option[String] = None)"
    compare(c1, expected)
  }
  
  def compare(t: Tree, t2: Tree) = {
    println(t)
    println(t2)
    t.structure shouldBe t2.structure
  }
}
