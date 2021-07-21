package tests.core

import com.github.javaparser.ast.`type`.PrimitiveType
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import dev.guardrail.generators.Java.Dropwizard
import dev.guardrail.generators.syntax.Java._
import dev.guardrail.{ Clients, Context }
import scala.collection.JavaConverters._
import support.SwaggerSpecRunner
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RequestBodiesTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner {
  val openapi: String =
    """
      |openapi: 3.0.2
      |info:
      |  version: 1.0.0
      |paths:
      |  /foo:
      |    post:
      |      x-jvm-package: requestBodies
      |      operationId: foo
      |      requestBody:
      |        $ref: "#/components/requestBodies/SomeRequestBodyForm"
      |      responses:
      |        200: {}
      |  /bar:
      |    post:
      |      x-jvm-package: requestBodies
      |      operationId: bar
      |      requestBody:
      |        $ref: "#/components/requestBodies/SomeRequestBodyWithRef"
      |      responses:
      |        200: {}
      |components:
      |  schemas:
      |    SomeSchema:
      |      type: object
      |      required:
      |        - a
      |      properties:
      |        a:
      |          type: string
      |        b:
      |          type: boolean
      |  requestBodies:
      |    SomeRequestBodyForm:
      |      required: true
      |      content:
      |        application/x-www-form-urlencoded:
      |          schema:
      |            required:
      |              - d
      |            properties:
      |              c:
      |                type: integer
      |                format: int64
      |              d:
      |                type: number
      |                format: double
      |    SomeRequestBodyWithRef:
      |      required: true
      |      content:
      |        application/json:
      |          schema:
      |            $ref: "#/components/schemas/SomeSchema"
    """.stripMargin

  "References to requestBodies should resolve and generate the proper args/methods" in {
    val (_, Clients(client :: _, _), _) =
      runSwaggerSpec(openapi)(Context.empty, Dropwizard)

    val cls = client.client.head.getOrElse(fail("Client does not contain a ClassDefinition"))

    val fooMethod = cls.getMethodsByName("foo").get(0)
    fooMethod.getParameter(0).getType shouldBe PrimitiveType.doubleType

    val fooCallBuilder = cls.getMembers.toList
      .collectFirst({
        case cbClass: ClassOrInterfaceDeclaration if cbClass.getNameAsString == "FooCallBuilder" => cbClass
      })
      .get
    val withCMethods = fooCallBuilder.getMethodsByName("withC").asScala
    withCMethods.length shouldBe 2
    withCMethods.foreach({
      case md if md.getParameter(0).getType.isPrimitiveType =>
        md.getParameter(0).getType shouldBe PrimitiveType.longType
      case md =>
        val paramType = md.getParameter(0).getType.asClassOrInterfaceType
        paramType.getNameAsString shouldBe "Optional"
        paramType.getTypeArguments.get.get(0).asClassOrInterfaceType.getNameAsString shouldBe "Long"
    })

    val barMethod          = cls.getMethodsByName("bar").get(0)
    val barMethodParamType = barMethod.getParameter(0).getType.asClassOrInterfaceType
    barMethodParamType.getNameAsString shouldBe "SomeSchema"
  }
}
