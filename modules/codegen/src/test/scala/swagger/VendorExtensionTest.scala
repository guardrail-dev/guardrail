package swagger

import java.util

import dev.guardrail.core.extract.VendorExtension
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions

import scala.jdk.CollectionConverters._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class VendorExtensionTest extends AnyFunSuite with Matchers {

  val spec: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |produces:
    | - application/json
    |x-scala-garbage: io.swagger.models.Swagger
    |paths:
    |  foo:
    |    x-scala-garbage: io.swagger.models.Path
    |    get:
    |      x-scala-garbage: io.swagger.models.Operation
    |      parameters:
    |      - name: id
    |        in: query
    |        required: true
    |        type: integer
    |        x-scala-garbage: io.swagger.models.parameters.Parameter
    |      responses:
    |        '200':
    |          description: successful operation
    |          x-scala-garbage: io.swagger.models.Response
    |          schema:
    |            "$$ref": "#/definitions/Order"
    |        '404':
    |          description: Order not found
    |          x-scala-garbage: io.swagger.models.Response
    |definitions:
    |  Order:
    |    type: object
    |    x-scala-garbage: io.swagger.models.Model
    |    properties:
    |      id:
    |        type: integer
    |        format: int64
    |        x-scala-garbage: io.swagger.models.properties.Property
    |""".stripMargin

  test("Able to extract strings") {
    val parseOpts = new ParseOptions
    parseOpts.setResolve(true)
    val swagger = new OpenAPIParser().readContents(spec, new util.LinkedList(), parseOpts).getOpenAPI
    // VendorExtension(swagger).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Swagger"))
    for {
      (k, v) <- swagger.getPaths.asScala
      _ = VendorExtension(v).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Path"))
      op <- v.readOperations().asScala
      _ = VendorExtension(op).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Operation"))
      _ = for {
        param <- op.getResponses.asScala.values
        // _ = VendorExtension(param).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.parameters.Parameter"))
      } ()
      _ = for {
        (_, resp) <- op.getResponses.asScala
        _ = VendorExtension(resp)
          .extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Response"))
      } ()
    } ()

    for {
      (_, defn) <- swagger.getComponents.getSchemas.asScala
      _ = VendorExtension(defn).extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.Model"))
      _ = for {
        (_, prop) <- defn.getProperties.asScala
        _ = VendorExtension(prop)
          .extract[String]("x-scala-garbage") should equal(Some("io.swagger.models.properties.Property"))
      } ()
    } ()
  }
}
