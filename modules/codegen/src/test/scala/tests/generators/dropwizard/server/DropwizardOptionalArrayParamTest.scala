package tests.generators.dropwizard.server

import com.github.javaparser.ast.body.MethodDeclaration
import dev.guardrail.{ Context, Server, Servers }
import dev.guardrail.generators.Java.Dropwizard
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._
import support.SwaggerSpecRunner

class DropwizardOptionalArrayParamTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner {
  private val openapi =
    """openapi: 3.0.2
      |info:
      |  title: Foo
      |  version: 1.0.0
      |paths:
      |  /foo:
      |    get:
      |      operationId: getFoo
      |      parameters:
      |        - name: optstuff
      |          in: query
      |          required: false
      |          schema:
      |            type: array
      |            items:
      |              type: string
      |        - name: reqstuff
      |          in: query
      |          required: true
      |          schema:
      |            type: array
      |            items:
      |              type: string
      |      responses:
      |        200: {}
      |""".stripMargin

  "Optional array resource method params should be unwrapped" in {
    val (
      _,
      _,
      Servers(Server(_, _, _, genResource :: Nil) :: Nil, _)
    ) = runSwaggerSpec(openapi)(Context.empty, Dropwizard)

    val getFooMethod = genResource
      .asClassOrInterfaceDeclaration()
      .getMembers
      .asScala
      .collectFirst({
        case method: MethodDeclaration if method.getNameAsString == "getFoo" => method
      })
      .value

    getFooMethod
      .getParameterByName("optstuff")
      .asScala
      .value
      .getType
      .asClassOrInterfaceType
      .getName
      .getIdentifier mustBe "List"

    getFooMethod
      .getParameterByName("reqstuff")
      .asScala
      .value
      .getType
      .asClassOrInterfaceType
      .getName
      .getIdentifier mustBe "List"
  }
}
