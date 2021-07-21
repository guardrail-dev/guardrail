package tests.generators.dropwizard.server

import com.github.javaparser.ast.body.MethodDeclaration
import dev.guardrail.generators.Java.Dropwizard
import dev.guardrail.{ Context, Server, Servers }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters._
import support.SwaggerSpecRunner

class DropwizardContentTypesTest extends AnyFreeSpec with Matchers with SwaggerSpecRunner {
  private val swagger: String =
    s"""
       |openapi: 3.0.1
       |paths:
       |  /foo:
       |    post:
       |      operationId: foo
       |      responses:
       |        204: {}
       |        404:
       |          content:
       |            application/json:
       |              schema:
       |                type: object
       |  /foo-multiple:
       |    post:
       |      operationId: fooMultiple
       |      responses:
       |        204:
       |          content:
       |            application/json:
       |              schema:
       |                type: object
       |        404:
       |          content:
       |            text/plain:
       |              schema:
       |                type: string
       |""".stripMargin

  "Produces annotation should still be added when success response has no body, but errors do" in {
    val (
      _,
      _,
      Servers(Server(_, _, _, genResource :: Nil) :: Nil, _)
    ) = runSwaggerSpec(swagger)(Context.empty, Dropwizard)

    genResource
      .asClassOrInterfaceDeclaration()
      .getMembers
      .asScala
      .collectFirst({
        case method: MethodDeclaration if method.getNameAsString == "foo" => method
      })
      .value
      .getAnnotationByName("Produces")
      .asScala
      .value
      .asSingleMemberAnnotationExpr()
      .getMemberValue
      .toString mustBe "MediaType.APPLICATION_JSON"
  }

  // This doesn't yet work because when the core threads through response info, we lose which content-type
  // is associated with which response.  But I'll leave the test here to re-enable later when we fix that.
  /*
  "Produces annotation should have multiple members when different resposes have different content types" in {
    val (
      _,
      _,
      Servers(Server(_, _, _, genResource :: Nil) :: Nil, _)
    ) = runSwaggerSpec(swagger)(Context.empty, Dropwizard)

    val annotationArrayValues = genResource
      .asClassOrInterfaceDeclaration()
      .getMembers
      .asScala
      .collectFirst({
        case method: MethodDeclaration if method.getNameAsString == "fooMultiple" => method
      })
      .get
      .getAnnotationByName("Produces")
      .get
      .asSingleMemberAnnotationExpr()
      .getMemberValue
      .asArrayInitializerExpr()
      .getValues
      .toList

    annotationArrayValues.length mustBe 2
    annotationArrayValues.find(_.toString == "MediaType.APPLICATION.JSON") mustNot be(None)
    annotationArrayValues.find(_.toString == "MediaType.TEXT_PLAIN") mustNot be(None)
  }
 */
}
