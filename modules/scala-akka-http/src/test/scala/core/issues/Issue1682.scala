package core.issues

import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.Context
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue1682 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  val swagger: String = s"""
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

  test("Support for HTTP HEAD method") {
    runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
  }
}
