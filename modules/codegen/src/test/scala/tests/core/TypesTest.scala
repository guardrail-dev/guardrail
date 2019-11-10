package tests.core

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import scala.meta._
import support.SwaggerSpecRunner

class TypesTest extends FunSuite with Matchers with SwaggerSpecRunner {

  test("Generate no definitions") {
    val swagger: String = s"""
      |swagger: "2.0"
      |info:
      |  title: Whatever
      |  version: 1.0.0
      |host: localhost:1234
      |definitions:
      |  Types:
      |    type: object
      |    properties:
      |      array:
      |        type: array
      |        items:
      |          type: boolean
      |      map:
      |        type: object
      |        additionalProperties:
      |          type: boolean
      |      obj:
      |        type: object
      |      bool:
      |        type: boolean
      |      string:
      |        type: string
      |      date:
      |        type: string
      |        format: date
      |      date_time:
      |        type: string
      |        format: date-time
      |      long:
      |        type: integer
      |        format: int64
      |      int:
      |        type: integer
      |        format: int32
      |      float:
      |        type: number
      |        format: float
      |      double:
      |        type: number
      |        format: double
      |      number:
      |        type: number
      |      integer:
      |        type: integer
      |      untyped:
      |        description: Untyped
      |      custom:
      |        type: string
      |        x-scala-type: Foo
      |      customComplex:
      |        type: string
      |        x-scala-type: Foo[Bar]
      |      nested:
      |        type: object
      |        properties:
      |          prop1:
      |            type: string
      |      nestedArray:
      |        type: array
      |        items:
      |          type: object
      |          properties:
      |            prop1:
      |              type: string
      |      requiredArray:
      |        type: array
      |        items:
      |          type: string
      |    required:
      |      - requiredArray
      |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Types(
        array: scala.Option[IndexedSeq[Boolean]] = scala.None,
        map: scala.Option[Map[String, Boolean]] = scala.None,
        obj: scala.Option[io.circe.Json] = scala.None,
        bool: scala.Option[Boolean] = scala.None,
        string: scala.Option[String] = scala.None,
        date: scala.Option[java.time.LocalDate] = scala.None,
        date_time: scala.Option[java.time.OffsetDateTime] = scala.None,
        long: scala.Option[Long] = scala.None,
        int: scala.Option[Int] = scala.None,
        float: scala.Option[Float] = scala.None,
        double: scala.Option[Double] = scala.None,
        number: scala.Option[BigDecimal] = scala.None,
        integer: scala.Option[BigInt] = scala.None,
        untyped: scala.Option[io.circe.Json] = scala.None,
        custom: scala.Option[Foo] = scala.None,
        customComplex: scala.Option[Foo[Bar]] = scala.None,
        nested: scala.Option[Types.Nested] = scala.None,
        nestedArray: scala.Option[IndexedSeq[Types.NestedArray]] = scala.None,
        requiredArray: IndexedSeq[String] = IndexedSeq.empty
      )
    """

    val companion = q"""
      object Types {
        implicit val encodeTypes: ObjectEncoder[Types] = {
          val readOnlyKeys = Set[String]()
            Encoder.forProduct19("array", "map", "obj", "bool", "string", "date", "date_time", "long", "int", "float", "double", "number", "integer", "untyped", "custom", "customComplex", "nested", "nestedArray", "requiredArray") ( (o: Types) => (o.array, o.map, o.obj, o.bool, o.string, o.date, o.date_time, o.long, o.int, o.float, o.double, o.number, o.integer, o.untyped, o.custom, o.customComplex, o.nested, o.nestedArray, o.requiredArray) ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeTypes: Decoder[Types] = Decoder.forProduct19("array", "map", "obj", "bool", "string", "date", "date_time", "long", "int", "float", "double", "number", "integer", "untyped", "custom", "customComplex", "nested", "nestedArray", "requiredArray")(Types.apply _)

        case class Nested(prop1: scala.Option[String] = scala.None)
        object Nested {
          implicit val encodeNested: ObjectEncoder[Nested] = {
            val readOnlyKeys = Set[String]()
            Encoder.forProduct1("prop1")((o: Nested) => o.prop1).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeNested: Decoder[Nested] = Decoder.forProduct1("prop1")(Nested.apply _)
        }

        case class NestedArray(prop1: scala.Option[String] = scala.None)
        object NestedArray {
          implicit val encodeNestedArray: ObjectEncoder[NestedArray] = {
            val readOnlyKeys = Set[String]()
            Encoder.forProduct1("prop1")((o: NestedArray) => o.prop1).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeNestedArray: Decoder[NestedArray] = Decoder.forProduct1("prop1")(NestedArray.apply _)
        }
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }

  test("Generates from composed schema") {
    val swagger: String = s"""
      |swagger: "2.0"
      |info:
      |  title: Whatever
      |  version: 1.0.0
      |host: localhost:1234
      |definitions:
      |  That:
      |    type: object
      |    properties:
      |      string:
      |        type: string
      |  Types:
      |    type: object
      |    properties:
      |      composed:
      |        allOf:
      |          - $$ref: '#/definitions/That'
      |          - type: object
      |            properties:
      |              int:
      |                type: integer
      |                format: int32
      |
      |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, _, _) :: ClassDefinition(_, _, cls, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""case class Types(composed: scala.Option[Types.Composed] = scala.None)"""

    val companion = q"""
      object Types {
        implicit val encodeTypes: ObjectEncoder[Types] = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct1("composed")((o: Types) => o.composed).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeTypes: Decoder[Types] = Decoder.forProduct1("composed")(Types.apply _)
        case class Composed(string: scala.Option[String] = scala.None, int: scala.Option[Int] = scala.None)
        object Composed {
          implicit val encodeComposed: ObjectEncoder[Composed] = {
            val readOnlyKeys = Set[String]()
            Encoder.forProduct2("string", "int")((o: Composed) => (o.string, o.int)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeComposed: Decoder[Composed] = Decoder.forProduct2("string", "int")(Composed.apply _)
        }
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }

  test("Deeply nested structure is generated") {
    val swagger =
      s"""
      |swagger: "2.0"
      |info:
      |  title: Whatever
      |  version: 1.0.0
      |host: localhost:1234
      |definitions:
      |  First:
      |    type: object
      |    properties:
      |      Second:
      |        type: object
      |        properties:
      |          Third:
      |            type: object
      |            properties:
      |              Fourth:
      |                type: string
      |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""case class First(Second: scala.Option[First.Second] = scala.None)"""

    val companion = q"""
       object First {
         implicit val encodeFirst: ObjectEncoder[First] = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct1("Second")((o: First) => o.Second).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
         }
         implicit val decodeFirst: Decoder[First] = Decoder.forProduct1("Second")(First.apply _)
         case class Second(Third: scala.Option[First.Second.Third] = scala.None)
         object Second {
           implicit val encodeSecond: ObjectEncoder[Second] = {
             val readOnlyKeys = Set[String]()
             Encoder.forProduct1("Third")((o: Second) => o.Third).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
           }
           implicit val decodeSecond: Decoder[Second] = Decoder.forProduct1("Third")(Second.apply _)
           case class Third(Fourth: scala.Option[String] = scala.None)
           object Third {
             implicit val encodeThird: ObjectEncoder[Third] = {
               val readOnlyKeys = Set[String]()
               Encoder.forProduct1("Fourth")((o: Third) => o.Fourth).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
             }
             implicit val decodeThird: Decoder[Third] = Decoder.forProduct1("Fourth")(Third.apply _)
           }
         }
       }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }

  test("Inherited nested structure work") {
    val swagger =
      s"""
         |swagger: "2.0"
         |info:
         |  title: Whatever
         |  version: 1.0.0
         |host: localhost:1234
         |definitions:
         |  First:
         |    allOf:
         |    - $$ref: '#/definitions/Second'
         |  Second:
         |    type: object
         |    properties:
         |      value:
         |        type: string
         |      nested:
         |        type: object
         |        properties:
         |          value:
         |            type: boolean
         |""".stripMargin
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, _, _) :: ClassDefinition(_, _, _, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val cmp = companionForStaticDefns(staticDefns)

    val companion =
      q"""
        object Second {
          implicit val encodeSecond: ObjectEncoder[Second] = {
            val readOnlyKeys = Set[String]()
            Encoder.forProduct2("value", "nested") ( (o: Second) => (o.value, o.nested) ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeSecond: Decoder[Second] = Decoder.forProduct2("value", "nested")(Second.apply _)
          case class Nested(value: scala.Option[Boolean] = scala.None)
          object Nested {
            implicit val encodeNested: ObjectEncoder[Nested] = {
              val readOnlyKeys = Set[String]()
              Encoder.forProduct1("value") ((o: Nested) => o.value ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
            implicit val decodeNested: Decoder[Nested] = Decoder.forProduct1("value")(Nested.apply _)
          }
        }
       """
    val definition = q"""case class First(value: scala.Option[String] = scala.None, nested: scala.Option[Second.Nested] = scala.None)"""

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }
}
