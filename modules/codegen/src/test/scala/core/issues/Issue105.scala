package tests.core.issues

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{
  ClassDefinition,
  Client,
  ClientGenerator,
  Clients,
  CodegenApplication,
  Context,
  ProtocolDefinitions,
  ProtocolGenerator,
  RandomType,
  Target
}
import com.twilio.guardrail.tests._
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue105 extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  Foo:
    |    type: object
    |    properties:
    |      nonEmptyString:
    |        type: string
    |        format: int64
    |        x-scala-type: String Refined NonEmpty
    |      positiveLong:
    |        type: integer
    |        format: int64
    |        x-scala-type: Long Refined Positive
    |""".stripMargin

  test("Generate plain array alias definition") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, cmp) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val definition = q"""
      case class Foo(nonEmptyString: Option[String Refined NonEmpty] = None, positiveLong: Option[Long Refined Positive] = None)
    """

    val companion = q"""
      object Foo {
        implicit val encodeFoo = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct2("nonEmptyString", "positiveLong")((o: Foo) => (o.nonEmptyString, o.positiveLong) ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFoo = Decoder.forProduct2("nonEmptyString", "positiveLong")(Foo.apply _)
      }
    """

    cls.structure shouldBe definition.structure
    cmp.structure shouldBe companion.structure
  }
}
