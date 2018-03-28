package tests.core

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger._
import com.twilio.swagger.codegen.generators.AkkaHttp
import com.twilio.swagger.codegen.{ClassDefinition, ProtocolGenerator, CodegenApplication, Target}
import org.scalatest.{FunSuite, Matchers}
import scala.meta._

class ScalaTypesTest extends FunSuite with Matchers {

  val swagger = new SwaggerParser().parse(s"""
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
    |""".stripMargin)

  test("Generate no definitions") {
    val definitions = Target.unsafeExtract(ProtocolGenerator.fromSwagger[CodegenApplication](swagger).foldMap(AkkaHttp)).elems
    definitions.length should equal (1)

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

    val ClassDefinition(_, _, cls, cmp) = definitions.head
    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }
}
