package tests.core.issues

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import scala.meta._
import support.SwaggerSpecRunner

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
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Foo(nonEmptyString: Option[String Refined NonEmpty] = None, positiveLong: Option[Long Refined Positive] = None)
    """

    val companion = q"""
      object Foo {
        implicit val encodeFoo: Encoder.AsObject[Foo] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[Foo](a => JsonObject.fromIterable(Vector(("nonEmptyString", a.nonEmptyString.asJson), ("positiveLong", a.positiveLong.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFoo: Decoder[Foo] = new Decoder[Foo] { final def apply(c: HCursor): Decoder.Result[Foo] = for (v0 <- c.downField("nonEmptyString").as[Option[String Refined NonEmpty]]; v1 <- c.downField("positiveLong").as[Option[Long Refined Positive]]) yield Foo(v0, v1) }
      }
    """

    cls.structure shouldBe definition.structure
    cmp.structure shouldBe companion.structure
  }
}
