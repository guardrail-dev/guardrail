package core.issues

import com.twilio.guardrail.{Context, Server, Servers}
import com.twilio.guardrail.generators.Http4s
import org.scalatest.{FunSpec, Matchers}
import support.SwaggerSpecRunner

class Issue290 extends FunSpec with Matchers with SwaggerSpecRunner {

  describe("Multiple date query params generate ambiguous implicit values") {

    val specification = """
      | openapi: "3.0.0"
      | info:
      |   title: Generator Error Sample
      |   version: 1.0.0
      | paths:
      |   /events:
      |     parameters:
      |       - $ref: '#/components/parameters/FromParam'
      |       - $ref: '#/components/parameters/ToParam'
      |     get:
      |       operationId: getEvents
      |       responses:
      |         200:
      |           description: Requested events
      | components:
      |   parameters:
      |     FromParam:
      |       name: from
      |       in: query
      |       schema:
      |         type: string
      |         format: date
      |     ToParam:
      |       name: to
      |       in: query
      |       schema:
      |         type: string
      |         format: date""".stripMargin

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
    ) = runSwaggerSpec(specification)(Context.empty, Http4s)

    it("Ensures that only one implicit value is generated for the LocalDate param") {

      val pattern = "implicit val .*QueryParamDecoder\\[java.time.LocalDate\\]".r

      val hits = pattern.findAllIn(serverDefinition.toString()).length

      hits shouldBe 1
    }
  }
}
