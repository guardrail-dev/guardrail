package core.issues

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.ClassDefinition

class Issue370 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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

  def testVersion(version: Http4sVersion): Unit =
    test(s"$version - Test nested enum definition") {
      val (
        ProtocolDefinitions(ClassDefinition(_, _, _, c1, s, _) :: _, _, _, _, _),
        _,
        _
      ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      val cmp = companionForStaticDefns(s)

      val companion =
        q"""
        object Foo {
          implicit val encodeFoo: _root_.io.circe.Encoder.AsObject[Foo] = {
            val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String]()
            _root_.io.circe.Encoder.AsObject.instance[Foo](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("value", a.value.asJson), ("value2", a.value2.asJson), ("nested", a.nested.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeFoo: _root_.io.circe.Decoder[Foo] = new _root_.io.circe.Decoder[Foo] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Foo] = for (v0 <- c.downField("value").as[Foo.Value]; v1 <- c.downField("value2").as[Baz]; v2 <- c.downField("nested").as[Option[Foo.Nested]]) yield Foo(v0, v1, v2) }
          sealed abstract class Value(val value: String) extends _root_.scala.Product with _root_.scala.Serializable { override def toString: String = value.toString }
          object Value {
            object members {
              case object A extends Value("a")
              case object B extends Value("b")
            }
            val A: Value = members.A
            val B: Value = members.B
            val values = _root_.scala.Vector(A, B)
            implicit val encodeValue: _root_.io.circe.Encoder[Value] = _root_.io.circe.Encoder[String].contramap(_.value)
            implicit val decodeValue: _root_.io.circe.Decoder[Value] = _root_.io.circe.Decoder[String].emap(value => from(value).toRight(s"$$value not a member of Value"))
            implicit val showValue: Show[Value] = Show[String].contramap[Value](_.value)
            def from(value: String): _root_.scala.Option[Value] = values.find(_.value == value)
            implicit val order: cats.Order[Value] = cats.Order.by[Value, Int](values.indexOf)
          }
          case class Nested(value: Foo.Nested.Value = Foo.Nested.Value.C)
          object Nested {
            implicit val encodeNested: _root_.io.circe.Encoder.AsObject[Nested] = {
              val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String]()
              _root_.io.circe.Encoder.AsObject.instance[Nested](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("value", a.value.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
            implicit val decodeNested: _root_.io.circe.Decoder[Nested] = new _root_.io.circe.Decoder[Nested] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Nested] = for (v0 <- c.downField("value").as[Foo.Nested.Value]) yield Nested(v0) }
            sealed abstract class Value(val value: String) extends _root_.scala.Product with _root_.scala.Serializable { override def toString: String = value.toString }
            object Value {
              object members {
                case object C extends Value("c")
                case object D extends Value("d")
              }
              val C: Value = members.C
              val D: Value = members.D
              val values = _root_.scala.Vector(C, D)
              implicit val encodeValue: _root_.io.circe.Encoder[Value] = _root_.io.circe.Encoder[String].contramap(_.value)
              implicit val decodeValue: _root_.io.circe.Decoder[Value] = _root_.io.circe.Decoder[String].emap(value => from(value).toRight(s"$$value not a member of Value"))
              implicit val showValue: Show[Value] = Show[String].contramap[Value](_.value)
              def from(value: String): _root_.scala.Option[Value] = values.find(_.value == value)
              implicit val order: cats.Order[Value] = cats.Order.by[Value, Int](values.indexOf)
            }
          }
        }
       """

      c1.structure shouldEqual q"case class Foo(value: Foo.Value = Foo.Value.A, value2: Baz = Baz.X, nested: Option[Foo.Nested] = None)".structure
      companion.structure shouldEqual cmp.structure
    }

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
