package swagger
package protocols

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class BigObjectSpec extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  BigObject:
    |    type: object
    |    properties:
    |      v1:
    |        type: integer
    |        format: int32
    |      v2:
    |        type: integer
    |        format: int32
    |      v3:
    |        type: integer
    |        format: int32
    |      v4:
    |        type: integer
    |        format: int32
    |      v5:
    |        type: integer
    |        format: int32
    |      v6:
    |        type: integer
    |        format: int32
    |      v7:
    |        type: integer
    |        format: int32
    |      v8:
    |        type: integer
    |        format: int32
    |      v9:
    |        type: integer
    |        format: int32
    |      v10:
    |        type: integer
    |        format: int32
    |      v11:
    |        type: integer
    |        format: int32
    |      v12:
    |        type: integer
    |        format: int32
    |      v13:
    |        type: integer
    |        format: int32
    |      v14:
    |        type: integer
    |        format: int32
    |      v15:
    |        type: integer
    |        format: int32
    |      v16:
    |        type: integer
    |        format: int32
    |      v17:
    |        type: integer
    |        format: int32
    |      v18:
    |        type: integer
    |        format: int32
    |      v19:
    |        type: integer
    |        format: int32
    |      v20:
    |        type: integer
    |        format: int32
    |      v21:
    |        type: integer
    |        format: int32
    |      v22:
    |        type: integer
    |        format: int32
    |      v23:
    |        type: integer
    |        format: int32
    |      v24:
    |        type: integer
    |        format: int32
    |      v25:
    |        type: integer
    |        format: int32
    |      v26:
    |        type: integer
    |        format: int32
    |      v27:
    |        type: integer
    |        format: int32
    |      v28:
    |        type: integer
    |        format: int32
    |      v29:
    |        type: integer
    |        format: int32
    |      v30:
    |        type: integer
    |        format: int32
    |""".stripMargin

  test("Big objects can be generated") {
    val (ProtocolDefinitions(ClassDefinition(_, _, cls, staticDefns, _) :: Nil, _, _, _), _, _) =
      runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class BigObject(v1: scala.Option[Int] = scala.None, v2: scala.Option[Int] = scala.None, v3: scala.Option[Int] = scala.None, v4: scala.Option[Int] = scala.None, v5: scala.Option[Int] = scala.None, v6: scala.Option[Int] = scala.None, v7: scala.Option[Int] = scala.None, v8: scala.Option[Int] = scala.None, v9: scala.Option[Int] = scala.None, v10: scala.Option[Int] = scala.None, v11: scala.Option[Int] = scala.None, v12: scala.Option[Int] = scala.None, v13: scala.Option[Int] = scala.None, v14: scala.Option[Int] = scala.None, v15: scala.Option[Int] = scala.None, v16: scala.Option[Int] = scala.None, v17: scala.Option[Int] = scala.None, v18: scala.Option[Int] = scala.None, v19: scala.Option[Int] = scala.None, v20: scala.Option[Int] = scala.None, v21: scala.Option[Int] = scala.None, v22: scala.Option[Int] = scala.None, v23: scala.Option[Int] = scala.None, v24: scala.Option[Int] = scala.None, v25: scala.Option[Int] = scala.None, v26: scala.Option[Int] = scala.None, v27: scala.Option[Int] = scala.None, v28: scala.Option[Int] = scala.None, v29: scala.Option[Int] = scala.None, v30: scala.Option[Int] = scala.None)
    """
    val companion  = q"""
      object BigObject {
        implicit val encodeBigObject: ObjectEncoder[BigObject] = {
          val readOnlyKeys = Set[String]()
          new ObjectEncoder[BigObject] { final def encodeObject(a: BigObject): JsonObject = JsonObject.fromIterable(Vector(("v1", a.v1.asJson), ("v2", a.v2.asJson), ("v3", a.v3.asJson), ("v4", a.v4.asJson), ("v5", a.v5.asJson), ("v6", a.v6.asJson), ("v7", a.v7.asJson), ("v8", a.v8.asJson), ("v9", a.v9.asJson), ("v10", a.v10.asJson), ("v11", a.v11.asJson), ("v12", a.v12.asJson), ("v13", a.v13.asJson), ("v14", a.v14.asJson), ("v15", a.v15.asJson), ("v16", a.v16.asJson), ("v17", a.v17.asJson), ("v18", a.v18.asJson), ("v19", a.v19.asJson), ("v20", a.v20.asJson), ("v21", a.v21.asJson), ("v22", a.v22.asJson), ("v23", a.v23.asJson), ("v24", a.v24.asJson), ("v25", a.v25.asJson), ("v26", a.v26.asJson), ("v27", a.v27.asJson), ("v28", a.v28.asJson), ("v29", a.v29.asJson), ("v30", a.v30.asJson))) }.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeBigObject: Decoder[BigObject] = new Decoder[BigObject] {
          final def apply(c: HCursor): Decoder.Result[BigObject] = for {
            v1 <- c.downField("v1").as[scala.Option[Int]]
            v2 <- c.downField("v2").as[scala.Option[Int]]
            v3 <- c.downField("v3").as[scala.Option[Int]]
            v4 <- c.downField("v4").as[scala.Option[Int]]
            v5 <- c.downField("v5").as[scala.Option[Int]]
            v6 <- c.downField("v6").as[scala.Option[Int]]
            v7 <- c.downField("v7").as[scala.Option[Int]]
            v8 <- c.downField("v8").as[scala.Option[Int]]
            v9 <- c.downField("v9").as[scala.Option[Int]]
            v10 <- c.downField("v10").as[scala.Option[Int]]
            v11 <- c.downField("v11").as[scala.Option[Int]]
            v12 <- c.downField("v12").as[scala.Option[Int]]
            v13 <- c.downField("v13").as[scala.Option[Int]]
            v14 <- c.downField("v14").as[scala.Option[Int]]
            v15 <- c.downField("v15").as[scala.Option[Int]]
            v16 <- c.downField("v16").as[scala.Option[Int]]
            v17 <- c.downField("v17").as[scala.Option[Int]]
            v18 <- c.downField("v18").as[scala.Option[Int]]
            v19 <- c.downField("v19").as[scala.Option[Int]]
            v20 <- c.downField("v20").as[scala.Option[Int]]
            v21 <- c.downField("v21").as[scala.Option[Int]]
            v22 <- c.downField("v22").as[scala.Option[Int]]
            v23 <- c.downField("v23").as[scala.Option[Int]]
            v24 <- c.downField("v24").as[scala.Option[Int]]
            v25 <- c.downField("v25").as[scala.Option[Int]]
            v26 <- c.downField("v26").as[scala.Option[Int]]
            v27 <- c.downField("v27").as[scala.Option[Int]]
            v28 <- c.downField("v28").as[scala.Option[Int]]
            v29 <- c.downField("v29").as[scala.Option[Int]]
            v30 <- c.downField("v30").as[scala.Option[Int]]
          } yield BigObject(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30)
        }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }
}
