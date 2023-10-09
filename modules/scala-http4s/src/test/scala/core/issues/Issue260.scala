package core.issues

import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.Context
import dev.guardrail.generators.{ Server, Servers }
import support.SwaggerSpecRunner
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class Issue260 extends AnyFunSpec with Matchers with SwaggerSpecRunner {

  def testVersion(version: Http4sVersion): Unit =
    describe(version.toString()) {
      describe("LocalDate path param is generated more than once") {

        val spec: String = """
      | openapi: "3.0.0"
      | info:
      |   title: Generator Error Sample
      |   version: 1.0.0
      | paths:
      |   /users/{userId}/occasions/{date}:
      |     parameters:
      |       - $ref: '#/components/parameters/UserIdParam'
      |       - $ref: '#/components/parameters/DateParam'
      |     get:
      |       operationId: getOccasionByDate
      |       responses:
      |         200:
      |           description: Requested occasion
      |           content:
      |             application/json:
      |               schema:
      |                 $ref: '#/components/schemas/OccasionResponseJson'
      |         404:
      |           description: Requested user or occasion was not found
      |     delete:
      |       operationId: deleteOccasionByDate
      |       responses:
      |         204:
      |           description: Succesfully deleted occasion
      |         404:
      |           description: Requested user or occasion was not found
      | components:
      |   parameters:
      |     UserIdParam:
      |       name: userId
      |       in: path
      |       description: Id of the specific user
      |       required: true
      |       schema:
      |         type: string
      |     DateParam:
      |       name: date
      |       in: path
      |       description: Date of the specific occasion
      |       required: true
      |       schema:
      |         type: string
      |         format: date
      |   schemas:
      |     OccasionResponseJson:
      |       type: object
      |       properties:
      |         date:
      |           type: string
      |           format: date
      |       required:
      |         - date""".stripMargin

        val (
          _, // ProtocolDefinitions
          _, // clients
          Servers(
            Server(
              _, // pkg
              _, // extraImports
              _, // genHandler
              serverDefinition :: _
            ) :: Nil,
            _ // supportDefinitions
          )
        ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, version.value)

        it("Ensure LocalDateVar is generated only once") {

          val pattern = "object LocalDateVar".r

          val hits = pattern.findAllIn(serverDefinition.toString()).length

          hits shouldBe 1
        }
      }
    }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
