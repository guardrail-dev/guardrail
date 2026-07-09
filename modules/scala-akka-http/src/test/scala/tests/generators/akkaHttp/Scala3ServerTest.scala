package tests.generators.akkaHttp

import dev.guardrail.Context
import dev.guardrail.generators.{ Server, Servers }
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.ScalaVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import support.{ ScalaMetaMatchers, SwaggerSpecRunner }

class Scala3ServerTest extends AnyFunSuite with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {
  import scala.meta._

  val spec: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |paths:
    |  "/store/order/{order_id}":
    |    get:
    |      tags:
    |      - store
    |      x-jvm-package: store
    |      operationId: getOrderById
    |      produces:
    |      - application/json
    |      parameters:
    |      - name: order_id
    |        in: path
    |        required: true
    |        type: integer
    |        format: int64
    |      responses:
    |        '200':
    |          description: successful operation
    |          schema:
    |            "$$ref": "#/definitions/Order"
    |        '404':
    |          description: Order not found
    |definitions:
    |  Order:
    |    type: object
    |    properties:
    |      id:
    |        type: integer
    |        format: int64
    |      status:
    |        type: string
    |""".stripMargin

  test("Ensure routes are generated with 'implicit' for Scala 2.13 (default)") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    val resourceSyntax = genResource.syntax

    // Should contain 'implicit' for Scala 2
    resourceSyntax should include("implicit mat: akka.stream.Materializer")
    resourceSyntax should include("implicit ec: scala.concurrent.ExecutionContext")

    // Should NOT contain 'using'
    resourceSyntax should not include "using mat:"
    resourceSyntax should not include "using ec:"
  }

  test("Ensure routes are generated with 'using' for Scala 3") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala3), "akka-http")

    val resourceSyntax = genResource.syntax

    // Should contain 'using' for Scala 3
    resourceSyntax should include("using mat: akka.stream.Materializer")
    resourceSyntax should include("using ec: scala.concurrent.ExecutionContext")

    // Should NOT contain 'implicit' in parameter clauses (but may contain in other places like implicit def)
    resourceSyntax should not include "implicit mat:"
    resourceSyntax should not include "implicit ec:"
  }

  test("Ensure Scala 2.12 generates 'implicit' syntax") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala212), "akka-http")

    val resourceSyntax = genResource.syntax

    // Should contain 'implicit' for Scala 2.12
    resourceSyntax should include("implicit mat: akka.stream.Materializer")
    resourceSyntax should not include "using mat:"
  }

  test("Ensure handler trait is the same for both Scala 2 and Scala 3") {
    val (_, _, Servers(Server(_, _, genHandler2, _) :: Nil, Nil)) =
      runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala213), "akka-http")

    val (_, _, Servers(Server(_, _, genHandler3, _) :: Nil, Nil)) =
      runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala3), "akka-http")

    // Handler trait should be identical - it doesn't have implicit/using clauses
    genHandler2.syntax shouldBe genHandler3.syntax
  }

  test("Ensure response TR methods use 'using' for Scala 3") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala3), "akka-http")

    val resourceSyntax = genResource.syntax

    // Response transform methods should use 'using ec:'
    resourceSyntax should include("using ec: scala.concurrent.ExecutionContext")
  }
}
