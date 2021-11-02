package tests.generators.dropwizard.server

import com.github.javaparser.ast.body.MethodDeclaration
import dev.guardrail.Context
import dev.guardrail.generators.{ Server, Servers }
import dev.guardrail.generators.java.dropwizard.Dropwizard
import org.scalatest.Retries
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.tagobjects.Retryable
import scala.jdk.CollectionConverters._
import scala.compat.java8.OptionConverters._
import support.SwaggerSpecRunner

class DropwizardOptionalArrayParamTest extends AnyFreeSpec with Matchers with Retries with SwaggerSpecRunner {

  override def withFixture(test: NoArgTest) =
    if (isRetryable(test))
      withRetry { super.withFixture(test) }
    else
      super.withFixture(test)

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

  "Optional array resource method params should be unwrapped" taggedAs (Retryable) in {
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
