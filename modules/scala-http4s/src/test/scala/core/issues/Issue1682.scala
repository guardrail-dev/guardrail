package core.issues

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail._
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion

class Issue1682 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  val spec: String = s"""
                           |swagger: '2.0'
                           |host: petstore.swagger.io
                           |paths:
                           |  "/pet/{petId}":
                           |    head:
                           |      operationId: test1682
                           |      tags:
                           |      - pet
                           |      parameters:
                           |      - name: petId
                           |        in: path
                           |        description: ID of pet that needs to be fetched
                           |        required: true
                           |        type: string
                           |      responses:
                           |        '400':
                           |          description: Invalid ID supplied
                           |        '404':
                           |          description: Pet not found
                           |""".stripMargin

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Test HTTP HEAD support") {
      runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, version.value)
    }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
