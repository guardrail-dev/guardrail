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
      ProtocolDefinitions(ClassDefinition(_, _, c1, s, _) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val companion = companionForStaticDefns(s)

    val cmp =
      q"""
        object Foo {
          implicit val encodeFoo: ObjectEncoder[Foo] = {
            val readOnlyKeys = Set[String]()
            Encoder.forProduct3("value", "value2", "nested") ( (o: Foo) => (o.value, o.value2, o.nested) ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeFoo: Decoder[Foo] = Decoder.forProduct3("value", "value2", "nested")(Foo.apply _)
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
          }
          case class Nested(value: scala.Option[Foo.Nested.Value] = scala.Option(Foo.Nested.Value.C))
          object Nested {
            implicit val encodeNested: ObjectEncoder[Nested] = {
              val readOnlyKeys = Set[String]()
              Encoder.forProduct1("value") ( (o: Nested) => o.value ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
            implicit val decodeNested: Decoder[Nested] = Decoder.forProduct1("value")(Nested.apply _)
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
            }
          }
        }
       """

    c1.structure shouldEqual q"case class Foo(value: scala.Option[Foo.Value] = scala.Option(Foo.Value.A), value2: Baz = Baz.X, nested: scala.Option[Foo.Nested] = scala.None)".structure
    companion.structure shouldEqual cmp.structure
  }
}
