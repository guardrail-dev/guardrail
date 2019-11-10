package tests.generators.akkaHttp

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import scala.meta._

class PropertyExtractors extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
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
      ProtocolDefinitions(ClassDefinition(_, _, cls, staticDefns, _) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Something(
        booleanValue: scala.Option[Boolean] = scala.None, stringValue: scala.Option[String] = scala.None,
        longProperty: scala.Option[Long] = scala.None, intProperty: scala.Option[Int] = scala.None,
        integerProperty: scala.Option[BigInt] = scala.None, floatProperty: scala.Option[Float] = scala.None,
        doubleProperty: scala.Option[Double] = scala.None, numberProperty: scala.Option[BigDecimal] = scala.None,
        untypedProperty: scala.Option[io.circe.Json] = scala.None,
        objectProperty: scala.Option[io.circe.Json] = scala.None
        /*, refProperty: scala.Option[ref_target_property] = scala.None, refTargetProperty: scala.Option[String] = scala.None,
        arrayProperty: scala.Option[IndexedSeq[ref_target_property]] = scala.Option(IndexedSeq.empty)
        */
      )
    """

    val companion = q"""
      object Something {
        implicit val encodeSomething: ObjectEncoder[Something] = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct10(
              "boolean_value", "string_value", "long_property", "int_property", "integer_property", "float_property",
              "double_property", "number_property", "untyped_property", "object_property"
              /*, "ref_property", "ref_target_property", "array_property" */
            )( (o: Something) => (
              o.booleanValue, o.stringValue, o.longProperty, o.intProperty, o.integerProperty, o.floatProperty,
              o.doubleProperty, o.numberProperty, o.untypedProperty, o.objectProperty
              /* , o.refProperty, o.refTargetProperty, o.arrayProperty */
            )
          ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeSomething: Decoder[Something] = Decoder.forProduct10(
          "boolean_value", "string_value", "long_property", "int_property", "integer_property", "float_property",
          "double_property", "number_property", "untyped_property", "object_property"
          /*, "ref_property", "ref_target_property", "array_property" */
        )(Something.apply _)
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
