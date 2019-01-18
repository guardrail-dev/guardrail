package tests.core.issues

import _root_.io.swagger.parser.SwaggerParser
import com.twilio.guardrail._
import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.languages.ScalaLanguage
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue166 extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger = s"""
                   |swagger: "2.0"
                   |info:
                   |  title: Whatever
                   |  version: 1.0.0
                   |host: localhost:1234
                   |schemes:
                   |  - http
                   |paths:
                   |  /blix:
                   |    get:
                   |      operationId: getBlix
                   |      responses:
                   |        200:
                   |          schema:
                   |            $$ref: "#/definitions/Blix"
                   |definitions:
                   |  Blix:
                   |    type: object
                   |    required:
                   |      - map
                   |    properties:
                   |      map:
                   |        type: string
                   |""".stripMargin

  test("Handle generation of models") {
    val (proto, codegen) = Target.unsafeExtract(
      Common
        .prepareDefinitions[ScalaLanguage, CodegenApplication[ScalaLanguage, ?]](
          CodegenTarget.Models,
          Context.empty,
          new SwaggerParser().parse(swagger)
        )
        .foldMap(Http4s)
    )

    val ProtocolDefinitions(ClassDefinition(_, _, cls, _, _) :: Nil, _, _, _) = proto
    val CodegenDefinitions(Nil, Nil)                                          = codegen

    val definition = q"""
      case class Blix(map: String)
    """

    cls.structure should equal(definition.structure)
  }

}
