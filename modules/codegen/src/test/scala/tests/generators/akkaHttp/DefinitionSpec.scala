package tests.generators.akkaHttp

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ ClassDefinition, Context, EnumDefinition, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import scala.meta._

class DefinitionSpec extends FunSuite with Matchers with SwaggerSpecRunner {

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
    |    x-jvm-package: com.twilio.whatever
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
      ProtocolDefinitions(ClassDefinition(_, _, cls, staticDefns, _) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class First(a: scala.Option[Int] = scala.None)
    """
    val companion  = q"""
      object First {
        implicit val encodeFirst: ObjectEncoder[First] = {
        val readOnlyKeys = Set[String]()
          Encoder.forProduct1("a")((o: First) => o.a).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFirst: Decoder[First] = Decoder.forProduct1("a")(First.apply _)
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Enumerations should be generated") {
    val (
      ProtocolDefinitions(_ :: _ :: EnumDefinition(_, _, _, cls, staticDefns) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
    sealed abstract class Third(val value: String) {
      override def toString: String = value.toString
    }
    """
    val companion  = q"""
    object Third {
      object members {
        case object V1 extends Third("v1")
        case object V2 extends Third("v2")
        case object ILikeSpaces extends Third("i like spaces")
      }
      val V1: Third = members.V1
      val V2: Third = members.V2
      val ILikeSpaces: Third = members.ILikeSpaces
      val values = Vector(V1, V2, ILikeSpaces)

      implicit val encodeThird: Encoder[Third] = Encoder[String].contramap(_.value)
      implicit val decodeThird: Decoder[Third] = Decoder[String].emap(value => parse(value).toRight(s"$${value} not a member of Third"))
      implicit val addPathThird: AddPath[Third] = AddPath.build(_.value)
      implicit val showThird: Show[Third] = Show.build(_.value)

      def parse(value: String): Option[Third] = values.find(_.value == value)
    }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Camel case conversion should happen") {
    val (
      ProtocolDefinitions(_ :: _ :: _ :: _ :: ClassDefinition(_, _, cls, staticDefns, _) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Fifth(aBCD: scala.Option[Int] = scala.None, bCDE: scala.Option[Int] = scala.None)
    """
    val companion  = q"""
      object Fifth {
        implicit val encodeFifth: ObjectEncoder[Fifth] = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct2("a_b_c_d", "b_c_d_e")((o: Fifth) => (o.aBCD, o.bCDE)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFifth: Decoder[Fifth] = Decoder.forProduct2("a_b_c_d", "b_c_d_e")(Fifth.apply _)
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Defaults should work") {
    val (
      ProtocolDefinitions(_ :: _ :: _ :: _ :: _ :: ClassDefinition(_, _, cls, staticDefns, _) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Sixth(defval: Int = 1, defvalOpt: scala.Option[Long] = scala.Option(2L))
    """
    val companion  = q"""
      object Sixth {
        implicit val encodeSixth: ObjectEncoder[Sixth] = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct2("defval", "defval_opt")((o: Sixth) => (o.defval, o.defvalOpt)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeSixth: Decoder[Sixth] = Decoder.forProduct2("defval", "defval_opt")(Sixth.apply _)
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
