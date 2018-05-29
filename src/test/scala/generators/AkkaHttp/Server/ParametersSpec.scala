package generators.AkkaHttp.Server

import com.twilio.guardrail.{Context, EnumDefinition, ProtocolDefinitions}
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.swagger.runSwaggerSpec
import org.scalatest.{FunSuite, Matchers}

class ParametersSpec extends FunSuite with Matchers{

  val swagger: String = s"""
   |swagger: "2.0"
   |info:
   |  title: Whatever
   |  version: 1.0.0
   |host: localhost:1234
   |schemes:
   |  - http
   |paths:
   |  /foo:
   |    get:
   |      operationId: getFoo
   |      produces:
   |        - application/json
   |      parameters:
   |        - name: p1
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p2
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p3
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p4
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p5
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p6
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p7
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p8
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p9
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p10
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p11
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p12
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p13
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p14
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p15
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p16
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p17
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p18
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p19
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p20
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p21
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p22
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p23
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p24
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |        - name: p25
   |          in: query
   |          required: true
   |          type: string
   |          x-scala-type: Bar
   |
   |      responses:
   |        200:
   |          description: Success
   |          schema:
   |            $$ref: "#/definitions/Bar"
   |definitions:
   |  Bar:
   |    type: string
   |    enum:
   |      - v1
   |      - v2
   |      - i like spaces
   |""".stripMargin

  test("generate server with 23 params"){

    val result = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)


    println(result)

  }

}
