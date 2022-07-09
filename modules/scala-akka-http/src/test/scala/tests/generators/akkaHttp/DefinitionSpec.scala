package tests.generators.akkaHttp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.{ ClassDefinition, EnumDefinition }

class DefinitionSpec extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  val swagger = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  First:
    |    type: object
    |    properties:
    |      a:
    |        type: integer
    |        format: int32
    |  Second:
    |    type: object
    |    properties:
    |      a:
    |        type: integer
    |        format: int32
    |  Third:
    |    type: string
    |    enum:
    |      - v1
    |      - v2
    |      - i like spaces
    |  Fourth:
    |    x-jvm-package: dev.guardrail.whatever
    |    type: string
    |    enum:
    |      - v1
    |      - v2
    |      - i like spaces
    |  Fifth:
    |    type: object
    |    properties:
    |      a_b_c_d:
    |        type: integer
    |        format: int32
    |      b_c_d_e:
    |        type: integer
    |        format: int32
    |  Sixth:
    |    type: object
    |    required:
    |      - defval
    |    properties:
    |      defval:
    |        type: integer
    |        format: int32
    |        default: 1
    |      defval_opt:
    |        type: integer
    |        format: int64
    |        default: 2
    |""".stripMargin

  test("Plain objects should be generated") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class First(a: Option[Int] = None)
    """
    val companion = q"""
      object First {
        implicit val encodeFirst: _root_.io.circe.Encoder.AsObject[First] = {
          val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String]()
          _root_.io.circe.Encoder.AsObject.instance[First](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("a", a.a.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFirst: _root_.io.circe.Decoder[First] = new _root_.io.circe.Decoder[First] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[First] = for (v0 <- c.downField("a").as[Option[Int]]) yield First(v0) }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Enumerations should be generated") {
    val (
      ProtocolDefinitions(_ :: _ :: EnumDefinition(_, _, _, _, cls, staticDefns) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
    sealed abstract class Third(val value: String) extends _root_.scala.Product with _root_.scala.Serializable {
      override def toString: String = value.toString
    }
    """
    val companion = q"""
    object Third {
      object members {
        case object V1 extends Third("v1")
        case object V2 extends Third("v2")
        case object ILikeSpaces extends Third("i like spaces")
      }
      val V1: Third = members.V1
      val V2: Third = members.V2
      val ILikeSpaces: Third = members.ILikeSpaces
      val values = _root_.scala.Vector(V1, V2, ILikeSpaces)
      implicit val encodeThird: _root_.io.circe.Encoder[Third] = _root_.io.circe.Encoder[String].contramap(_.value)
      implicit val decodeThird: _root_.io.circe.Decoder[Third] = _root_.io.circe.Decoder[String].emap(value => from(value).toRight(s"$$value not a member of Third"))
      implicit val showThird: Show[Third] = Show[String].contramap[Third](_.value)
      def from(value: String): _root_.scala.Option[Third] = values.find(_.value == value)
      implicit val order: cats.Order[Third] = cats.Order.by[Third, Int](values.indexOf)
    }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Camel case conversion should happen") {
    val (
      ProtocolDefinitions(_ :: _ :: _ :: _ :: ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Fifth(aBCD: Option[Int] = None, bCDE: Option[Int] = None)
    """
    val companion = q"""
      object Fifth {
        implicit val encodeFifth: _root_.io.circe.Encoder.AsObject[Fifth] = {
          val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String]()
          _root_.io.circe.Encoder.AsObject.instance[Fifth](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("a_b_c_d", a.aBCD.asJson), ("b_c_d_e", a.bCDE.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFifth: _root_.io.circe.Decoder[Fifth] = new _root_.io.circe.Decoder[Fifth] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Fifth] = for (v0 <- c.downField("a_b_c_d").as[Option[Int]]; v1 <- c.downField("b_c_d_e").as[Option[Int]]) yield Fifth(v0, v1) }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Defaults should work") {
    val (
      ProtocolDefinitions(_ :: _ :: _ :: _ :: _ :: ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Sixth(defval: Int = 1, defvalOpt: Long = 2L)
    """
    val companion = q"""
      object Sixth {
        implicit val encodeSixth: _root_.io.circe.Encoder.AsObject[Sixth] = {
          val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String]()
          _root_.io.circe.Encoder.AsObject.instance[Sixth](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("defval", a.defval.asJson), ("defval_opt", a.defvalOpt.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeSixth: _root_.io.circe.Decoder[Sixth] = new _root_.io.circe.Decoder[Sixth] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Sixth] = for (v0 <- c.downField("defval").as[Int]; v1 <- c.downField("defval_opt").as[Long]) yield Sixth(v0, v1) }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
