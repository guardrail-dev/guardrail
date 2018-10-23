package tests.core

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

import scala.meta._

class TypesTest extends FunSuite with Matchers with SwaggerSpecRunner {

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
    |        type: objet
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
    |""".stripMargin

  test("Generate no definitions") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, cls, cmp, _) :: Nil, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val definition = q"""
      case class Types(
        array: Option[IndexedSeq[Boolean]] = Option(IndexedSeq.empty),
        obj: Option[io.circe.Json] = None,
        bool: Option[Boolean] = None,
        string: Option[String] = None,
        date: Option[java.time.LocalDate] = None,
        date_time: Option[java.time.OffsetDateTime] = None,
        long: Option[Long] = None,
        int: Option[Int] = None,
        float: Option[Float] = None,
        double: Option[Double] = None,
        number: Option[BigDecimal] = None,
        integer: Option[BigInt] = None,
        untyped: Option[io.circe.Json] = None,
        custom: Option[Foo] = None,
        customComplex: Option[Foo[Bar]] = None
      )
    """

    val companion = q"""
      object Types {
        implicit val encodeTypes = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct15("array", "obj", "bool", "string", "date", "date_time", "long", "int", "float", "double", "number", "integer", "untyped", "custom", "customComplex")((o: Types) => (o.array, o.obj, o.bool, o.string, o.date, o.date_time, o.long, o.int, o.float, o.double, o.number, o.integer, o.untyped, o.custom, o.customComplex)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeTypes = Decoder.forProduct15("array", "obj", "bool", "string", "date", "date_time", "long", "int", "float", "double", "number", "integer", "untyped", "custom", "customComplex")(Types.apply _)
      }
    """

    cls.structure shouldEqual definition.structure
    cmp.structure shouldEqual companion.structure
  }
}
