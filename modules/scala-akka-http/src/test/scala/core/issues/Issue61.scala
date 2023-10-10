package tests.core.issues

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.terms.protocol.RandomType

class Issue61 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val spec: String = s"""
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
      ProtocolDefinitions(RandomType(_, tpe) :: _ :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    tpe.structure shouldBe t"Vector[String]".structure
  }

  test("Generate primitive type aliases") {
    val (
      ProtocolDefinitions(_ :: RandomType(_, tpe) :: Nil, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    tpe.structure shouldBe t"Long".structure
  }
}
