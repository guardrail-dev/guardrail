package tests.generators.akkaHttp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.terms.protocol.ClassDefinition

class PropertyExtractors extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val spec: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  Something:
    |    type: object
    |    required:
    |      - map
    |    properties:
    |      boolean_value:
    |        type: boolean
    |      string_value:
    |        type: string
    |      date_property:
    |        type: date
    |      date_time_property:
    |        type: date-time
    |      long_property:
    |        type: integer
    |        format: int64
    |      int_property:
    |        type: integer
    |        format: int32
    |      integer_property:
    |        type: integer
    |      float_property:
    |        type: number
    |        format: float
    |      double_property:
    |        type: number
    |        format: double
    |      number_property:
    |        type: number
    |      untyped_property:
    |        default: "what"
    |      object_property:
    |        type: object
    |""".stripMargin
  /*
    |      ref_property:
    |        "$$ref": "#/definitions/ref_target_property"
    |      ref_target_property:
    |        type: string
    |      array_property:
    |        type: array
    |        items:
    |          "$$ref": "#/definitions/ref_target_property"
   */

  test("Render all primitive types correctly") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Something(
        booleanValue: Option[Boolean] = None, stringValue: Option[String] = None,
        longProperty: Option[Long] = None, intProperty: Option[Int] = None,
        integerProperty: Option[BigInt] = None, floatProperty: Option[Float] = None,
        doubleProperty: Option[Double] = None, numberProperty: Option[BigDecimal] = None,
        untypedProperty: Option[io.circe.Json] = None,
        objectProperty: Option[io.circe.Json] = None
        /*, refProperty: Option[ref_target_property] = None, refTargetProperty: Option[String] = None,
        arrayProperty: Option[IndexedSeq[ref_target_property]] = Option(IndexedSeq.empty)
        */
      )
    """

    val companion = q"""
      object Something {
        implicit val encodeSomething: _root_.io.circe.Encoder.AsObject[Something] = {
          _root_.io.circe.Encoder.AsObject.instance[Something](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("boolean_value", a.booleanValue.asJson), ("string_value", a.stringValue.asJson), ("long_property", a.longProperty.asJson), ("int_property", a.intProperty.asJson), ("integer_property", a.integerProperty.asJson), ("float_property", a.floatProperty.asJson), ("double_property", a.doubleProperty.asJson), ("number_property", a.numberProperty.asJson), ("untyped_property", a.untypedProperty.asJson), ("object_property", a.objectProperty.asJson))))
        }
        implicit val decodeSomething: _root_.io.circe.Decoder[Something] = new _root_.io.circe.Decoder[Something] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Something] = for (v0 <- c.downField("boolean_value").as[Option[Boolean]]; v1 <- c.downField("string_value").as[Option[String]]; v2 <- c.downField("long_property").as[Option[Long]]; v3 <- c.downField("int_property").as[Option[Int]]; v4 <- c.downField("integer_property").as[Option[BigInt]]; v5 <- c.downField("float_property").as[Option[Float]]; v6 <- c.downField("double_property").as[Option[Double]]; v7 <- c.downField("number_property").as[Option[BigDecimal]]; v8 <- c.downField("untyped_property").as[Option[io.circe.Json]]; v9 <- c.downField("object_property").as[Option[io.circe.Json]]) yield Something(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9) }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
