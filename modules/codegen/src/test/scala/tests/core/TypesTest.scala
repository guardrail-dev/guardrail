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
      |      byte:
      |        type: string
      |        format: byte
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
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Types(
        array: Option[Vector[Boolean]] = None,
        map: Option[Map[String, Boolean]] = None,
        obj: Option[io.circe.Json] = None,
        bool: Option[Boolean] = None,
        string: Option[String] = None,
        date: Option[java.time.LocalDate] = None,
        date_time: Option[java.time.OffsetDateTime] = None,
        byte: Option[Base64String] = None,
        long: Option[Long] = None,
        int: Option[Int] = None,
        float: Option[Float] = None,
        double: Option[Double] = None,
        number: Option[BigDecimal] = None,
        integer: Option[BigInt] = None,
        untyped: Option[io.circe.Json] = None,
        custom: Option[Foo] = None,
        customComplex: Option[Foo[Bar]] = None,
        nested: Option[Types.Nested] = None,
        nestedArray: Option[Vector[Types.NestedArray]] = None,
        requiredArray: Vector[String] = Vector.empty
      )
    """

    val companion = q"""
      object Types {
        implicit val encodeTypes: Encoder.AsObject[Types] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[Types](a => JsonObject.fromIterable(Vector(("array", a.array.asJson), ("map", a.map.asJson), ("obj", a.obj.asJson), ("bool", a.bool.asJson), ("string", a.string.asJson), ("date", a.date.asJson), ("date_time", a.date_time.asJson), ("byte", a.byte.asJson), ("long", a.long.asJson), ("int", a.int.asJson), ("float", a.float.asJson), ("double", a.double.asJson), ("number", a.number.asJson), ("integer", a.integer.asJson), ("untyped", a.untyped.asJson), ("custom", a.custom.asJson), ("customComplex", a.customComplex.asJson), ("nested", a.nested.asJson), ("nestedArray", a.nestedArray.asJson), ("requiredArray", a.requiredArray.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeTypes: Decoder[Types] = new Decoder[Types] { final def apply(c: HCursor): Decoder.Result[Types] = for (v0 <- c.downField("array").as[Option[Vector[Boolean]]]; v1 <- c.downField("map").as[Option[Map[String, Boolean]]]; v2 <- c.downField("obj").as[Option[io.circe.Json]]; v3 <- c.downField("bool").as[Option[Boolean]]; v4 <- c.downField("string").as[Option[String]]; v5 <- c.downField("date").as[Option[java.time.LocalDate]]; v6 <- c.downField("date_time").as[Option[java.time.OffsetDateTime]]; v7 <- c.downField("byte").as[Option[Base64String]]; v8 <- c.downField("long").as[Option[Long]]; v9 <- c.downField("int").as[Option[Int]]; v10 <- c.downField("float").as[Option[Float]]; v11 <- c.downField("double").as[Option[Double]]; v12 <- c.downField("number").as[Option[BigDecimal]]; v13 <- c.downField("integer").as[Option[BigInt]]; v14 <- c.downField("untyped").as[Option[io.circe.Json]]; v15 <- c.downField("custom").as[Option[Foo]]; v16 <- c.downField("customComplex").as[Option[Foo[Bar]]]; v17 <- c.downField("nested").as[Option[Types.Nested]]; v18 <- c.downField("nestedArray").as[Option[Vector[Types.NestedArray]]]; v19 <- c.downField("requiredArray").as[Vector[String]]) yield Types(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19) }
        case class Nested(prop1: Option[String] = None)
        object Nested {
          implicit val encodeNested: Encoder.AsObject[Nested] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[Nested](a => JsonObject.fromIterable(Vector(("prop1", a.prop1.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeNested: Decoder[Nested] = new Decoder[Nested] { final def apply(c: HCursor): Decoder.Result[Nested] = for (v0 <- c.downField("prop1").as[Option[String]]) yield Nested(v0) }
        }
        case class NestedArray(prop1: Option[String] = None)
        object NestedArray {
          implicit val encodeNestedArray: Encoder.AsObject[NestedArray] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[NestedArray](a => JsonObject.fromIterable(Vector(("prop1", a.prop1.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeNestedArray: Decoder[NestedArray] = new Decoder[NestedArray] { final def apply(c: HCursor): Decoder.Result[NestedArray] = for (v0 <- c.downField("prop1").as[Option[String]]) yield NestedArray(v0) }
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
      ProtocolDefinitions(ClassDefinition(_, _, _, _, _, _) :: ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""case class Types(composed: Option[Types.Composed] = None)"""

    val companion = q"""
      object Types {
        implicit val encodeTypes: Encoder.AsObject[Types] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[Types](a => JsonObject.fromIterable(Vector(("composed", a.composed.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeTypes: Decoder[Types] = new Decoder[Types] { final def apply(c: HCursor): Decoder.Result[Types] = for (v0 <- c.downField("composed").as[Option[Types.Composed]]) yield Types(v0) }
        case class Composed(string: Option[String] = None, int: Option[Int] = None)
        object Composed {
          implicit val encodeComposed: Encoder.AsObject[Composed] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[Composed](a => JsonObject.fromIterable(Vector(("string", a.string.asJson), ("int", a.int.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeComposed: Decoder[Composed] = new Decoder[Composed] { final def apply(c: HCursor): Decoder.Result[Composed] = for (v0 <- c.downField("string").as[Option[String]]; v1 <- c.downField("int").as[Option[Int]]) yield Composed(v0, v1) }
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
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""case class First(Second: Option[First.Second] = None)"""

    val companion = q"""
      object First {
        implicit val encodeFirst: Encoder.AsObject[First] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[First](a => JsonObject.fromIterable(Vector(("Second", a.Second.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeFirst: Decoder[First] = new Decoder[First] { final def apply(c: HCursor): Decoder.Result[First] = for (v0 <- c.downField("Second").as[Option[First.Second]]) yield First(v0) }
        case class Second(Third: Option[First.Second.Third] = None)
        object Second {
          implicit val encodeSecond: Encoder.AsObject[Second] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[Second](a => JsonObject.fromIterable(Vector(("Third", a.Third.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeSecond: Decoder[Second] = new Decoder[Second] { final def apply(c: HCursor): Decoder.Result[Second] = for (v0 <- c.downField("Third").as[Option[First.Second.Third]]) yield Second(v0) }
          case class Third(Fourth: Option[String] = None)
          object Third {
            implicit val encodeThird: Encoder.AsObject[Third] = {
              val readOnlyKeys = Set[String]()
              Encoder.AsObject.instance[Third](a => JsonObject.fromIterable(Vector(("Fourth", a.Fourth.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
            implicit val decodeThird: Decoder[Third] = new Decoder[Third] { final def apply(c: HCursor): Decoder.Result[Third] = for (v0 <- c.downField("Fourth").as[Option[String]]) yield Third(v0) }
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
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, _, _) :: ClassDefinition(_, _, _, _, staticDefns, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val cmp = companionForStaticDefns(staticDefns)

    val companion =
      q"""
        object Second {
          implicit val encodeSecond: Encoder.AsObject[Second] = {
            val readOnlyKeys = Set[String]()
            Encoder.AsObject.instance[Second](a => JsonObject.fromIterable(Vector(("value", a.value.asJson), ("nested", a.nested.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
          }
          implicit val decodeSecond: Decoder[Second] = new Decoder[Second] { final def apply(c: HCursor): Decoder.Result[Second] = for (v0 <- c.downField("value").as[Option[String]]; v1 <- c.downField("nested").as[Option[Second.Nested]]) yield Second(v0, v1) }
          case class Nested(value: Option[Boolean] = None)
          object Nested {
            implicit val encodeNested: Encoder.AsObject[Nested] = {
              val readOnlyKeys = Set[String]()
              Encoder.AsObject.instance[Nested](a => JsonObject.fromIterable(Vector(("value", a.value.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
            }
            implicit val decodeNested: Decoder[Nested] = new Decoder[Nested] { final def apply(c: HCursor): Decoder.Result[Nested] = for (v0 <- c.downField("value").as[Option[Boolean]]) yield Nested(v0) }
          }
        }
       """
    val definition = q"""case class First(value: Option[String] = None, nested: Option[Second.Nested] = None)"""

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }
}
