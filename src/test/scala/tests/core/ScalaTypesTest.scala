package tests.core

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

import scala.meta._

class ScalaTypesTest extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |definitions:
    |  Baz:
    |    type: object
    |    properties:
    |      foo:
    |        type: string
    |        x-scala-type: com.twilio.foo.bar.Baz
    |""".stripMargin

  test("Generate no definitions") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, cmp, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val definition = q"""
      case class Baz(foo: Option[com.twilio.foo.bar.Baz] = None)
    """

    val companion = q"""
      object Baz {
        implicit val encodeBaz = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct1("foo")((o: Baz) => o.foo).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeBaz = Decoder.forProduct1("foo")(Baz.apply _)
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }
}
