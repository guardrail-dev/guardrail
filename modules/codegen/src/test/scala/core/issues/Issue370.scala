package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue370 extends FunSuite with Matchers with SwaggerSpecRunner {
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
                           |      value:
                           |        type: string
                           |        enum:
                           |          - a
                           |          - b
                           |        default: a
                           |      value2:
                           |        $$ref: "#/definitions/Baz"
                           |      nested:
                           |        type: object
                           |        properties:
                           |          value:
                           |           type: string
                           |           enum:
                           |             - c
                           |             - d
                           |           default: c
                           |    required:
                           |      - value2
                           |  Baz:
                           |    type: string
                           |    enum:
                           |      - x
                           |      - y
                           |    default: x
                           |""".stripMargin

  test("Test nested enum definition") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, c1, s, _) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val cmp = companionForStaticDefns(s)

    val companion =
      q"""
        object Foo {
          implicit val encodeFoo: Encoder.AsObject[Foo] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[Foo](a => JsonObject.fromIterable(Vector(("value", a.value.asJson), ("value2", a.value2.asJson), ("nested", a.nested.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeFoo: Decoder[Foo] = new Decoder[Foo] { final def apply(c: HCursor): Decoder.Result[Foo] = for (v0 <- c.downField("value").as[Option[Foo.Value]]; v1 <- c.downField("value2").as[Baz]; v2 <- c.downField("nested").as[Option[Foo.Nested]]) yield Foo(v0, v1, v2) }
          sealed abstract class Value(val value: String) { override def toString: String = value.toString }
          object Value {
            object members {
              case object A extends Value("a")
              case object B extends Value("b")
            }
            val A: Value = members.A
            val B: Value = members.B
            val values = Vector(A, B)
            implicit val encodeValue: Encoder[Value] = Encoder[String].contramap(_.value)
            implicit val decodeValue: Decoder[Value] = Decoder[String].emap(value => parse(value).toRight(s"$$value not a member of Value"))
            implicit val addPathValue: AddPath[Value] = AddPath.build(_.value)
            implicit val showValue: Show[Value] = Show.build(_.value)
            def parse(value: String): Option[Value] = values.find(_.value == value)
            implicit val order: cats.Order[Value] = cats.Order.by[Value, Int](values.indexOf)
          }
          case class Nested(value: Option[Foo.Nested.Value] = Option(Foo.Nested.Value.C))
          object Nested {
            implicit val encodeNested: Encoder.AsObject[Nested] = {
              val readOnlyKeys = Set[String]()
              Encoder.AsObject.instance[Nested](a => JsonObject.fromIterable(Vector(("value", a.value.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
            implicit val decodeNested: Decoder[Nested] = new Decoder[Nested] { final def apply(c: HCursor): Decoder.Result[Nested] = for (v0 <- c.downField("value").as[Option[Foo.Nested.Value]]) yield Nested(v0) }
            sealed abstract class Value(val value: String) { override def toString: String = value.toString }
            object Value {
              object members {
                case object C extends Value("c")
                case object D extends Value("d")
              }
              val C: Value = members.C
              val D: Value = members.D
              val values = Vector(C, D)
              implicit val encodeValue: Encoder[Value] = Encoder[String].contramap(_.value)
              implicit val decodeValue: Decoder[Value] = Decoder[String].emap(value => parse(value).toRight(s"$$value not a member of Value"))
              implicit val addPathValue: AddPath[Value] = AddPath.build(_.value)
              implicit val showValue: Show[Value] = Show.build(_.value)
              def parse(value: String): Option[Value] = values.find(_.value == value)
              implicit val order: cats.Order[Value] = cats.Order.by[Value, Int](values.indexOf)
            }
          }
        }
       """

    c1.structure shouldEqual q"case class Foo(value: Option[Foo.Value] = Option(Foo.Value.A), value2: Baz = Baz.X, nested: Option[Foo.Nested] = None)".structure
    companion.structure shouldEqual cmp.structure
  }
}
