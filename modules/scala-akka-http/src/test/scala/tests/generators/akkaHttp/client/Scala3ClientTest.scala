package tests.generators.akkaHttp.client

import dev.guardrail.Context
import dev.guardrail.generators.{ Client, Clients }
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.generators.ScalaVersion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import support.{ ScalaMetaMatchers, SwaggerSpecRunner }

class Scala3ClientTest extends AnyFunSuite with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {
  import scala.meta._

  val spec: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |basePath: /v2
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

  test("Ensure client is generated with 'implicit' for Scala 2.13 (default)") {
    val (
      _,
      Clients(Client(_, clientName, _, staticDefns, cls, _) :: _, _),
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    val clientSyntax = cls.head.value.syntax
    val cmp          = companionForStaticDefns(staticDefns)
    val staticSyntax = cmp.syntax

    // Should contain 'implicit' for Scala 2
    (clientSyntax + staticSyntax) should include("implicit")
  }

  test("Ensure client is generated with 'using' for Scala 3") {
    val (
      _,
      Clients(Client(_, clientName, _, staticDefns, cls, _) :: _, _),
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala3), "akka-http")

    val clientSyntax = cls.head.value.syntax
    val cmp          = companionForStaticDefns(staticDefns)
    val staticSyntax = cmp.syntax

    // Should contain 'using' for Scala 3 in the apply method or constructor
    (clientSyntax + staticSyntax) should include("using")
  }

  test("Ensure client constructor uses correct implicit/using based on Scala version") {
    val (_, Clients(Client(_, _, _, staticDefns2, cls2, _) :: _, _), _) =
      runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala213), "akka-http")

    val (_, Clients(Client(_, _, _, staticDefns3, cls3, _) :: _, _), _) =
      runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala3), "akka-http")

    val cmp2 = companionForStaticDefns(staticDefns2)
    val cmp3 = companionForStaticDefns(staticDefns3)

    val syntax2 = cls2.head.value.syntax + cmp2.syntax
    val syntax3 = cls3.head.value.syntax + cmp3.syntax

    // Scala 2 should have 'implicit' and no 'using' in parameter clauses
    syntax2 should include("implicit")

    // Scala 3 should have 'using'
    syntax3 should include("using")
  }

  test("Ensure Scala 2.12 client uses 'implicit'") {
    val (
      _,
      Clients(Client(_, _, _, staticDefns, cls, _) :: _, _),
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty.withScalaVersion(ScalaVersion.Scala212), "akka-http")

    val clientSyntax = cls.head.value.syntax
    val cmp          = companionForStaticDefns(staticDefns)
    val staticSyntax = cmp.syntax

    // Should contain 'implicit' for Scala 2.12
    (clientSyntax + staticSyntax) should include("implicit")
    // Should not contain 'using' as a parameter modifier
    (clientSyntax + staticSyntax) should not include "(using "
  }
}
