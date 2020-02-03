package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.{ ClassDefinition, Context, ProtocolDefinitions }
import org.scalatest.{ Assertion, FunSuite, Matchers }
import support.SwaggerSpecRunner

class Issue222 extends FunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
       |swagger: '2.0'
       |info:
       |  title: someapp
       |  description: someapp
       |  version: '1'
       |basePath: "/v1"
       |schemes:
       |  - http
       |produces:
       |  - application/json
       |paths: {}
       |definitions:
       |  Request:
       |    description: Request
       |    allOf:
       |      - "$$ref": "#/definitions/RequestFields"
       |      - type: object
       |        properties:
       |          id:
       |            type: string
       |  RequestFields:
       |    description: RequestFields
       |    type: object
       |    properties:
       |      state:
       |        type: integer
       |    required: [state]    
       |  Request2:
       |    description: Request2
       |    allOf:
       |      - "$$ref": "#/definitions/RequestFields2"
       |      - type: object
       |        properties:
       |          id:
       |            type: string
       |      - type: object
       |        properties:
       |          id2:
       |            type: string
       |        required: [id2]        
       |  RequestFields2:
       |    description: RequestFields2
       |    type: object
       |    properties:
       |      state2:
       |        type: integer
       |  Request3:
       |    description: Request3
       |    allOf:
       |      - "$$ref": "#/definitions/RequestFields"
       |      - type: object
       |        properties:
       |          id:
       |            type: string
       |      - type: object
       |        properties:
       |          id2:
       |            type: string
       |        required: [id2]
       |      - "$$ref": "#/definitions/RequestFields2"
       |""".stripMargin

  test("Ensure case-to-case inheritance is not generated") {
    val (x @ ProtocolDefinitions(List(request: ClassDefinition[ScalaLanguage], requestFields: ClassDefinition[ScalaLanguage], _, _, _), _, _, _), _, _) =
      runSwaggerSpec(swagger)(Context.empty, Http4s)

    val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

    val expectedRequestTpe = t"""Request"""

    val expectedRequestCls = q"""case class Request(state: BigInt, id: Option[String] = None)"""

    val expectedRequestEncoder =
      q"""
        implicit val encodeRequest: Encoder.AsObject[Request] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[Request](a => JsonObject.fromIterable(Vector(("state", a.state.asJson), ("id", a.id.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
      """
    val expectedRequestDecoder =
      q"""
        implicit val decodeRequest: Decoder[Request] = new Decoder[Request] { final def apply(c: HCursor): Decoder.Result[Request] = for (v0 <- c.downField("state").as[BigInt]; v1 <- c.downField("id").as[Option[String]]) yield Request(v0, v1) }
      """

    compare(request.tpe, expectedRequestTpe)
    compare(request.cls, expectedRequestCls)
    compare(reqEncoder, expectedRequestEncoder)
    compare(reqDecoder, expectedRequestDecoder)

    val expectedFieldsTpe = t"""RequestFields"""
    val expectedFieldsCls = q"""case class RequestFields(state: BigInt)"""

    val List(fieldsEncoder, fieldsDecoder) = requestFields.staticDefns.definitions

    val expectedFieldsEncoder =
      q"""
        implicit val encodeRequestFields: Encoder.AsObject[RequestFields] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[RequestFields](a => JsonObject.fromIterable(Vector(("state", a.state.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
      """
    val expectedFieldsDecoder =
      q"""
        implicit val decodeRequestFields: Decoder[RequestFields] = new Decoder[RequestFields] { final def apply(c: HCursor): Decoder.Result[RequestFields] = for (v0 <- c.downField("state").as[BigInt]) yield RequestFields(v0) }
      """

    compare(requestFields.tpe, expectedFieldsTpe)
    compare(requestFields.cls, expectedFieldsCls)
    compare(fieldsEncoder, expectedFieldsEncoder)
    compare(fieldsDecoder, expectedFieldsDecoder)
  }

  test("Ensure case-to-case inheritance is not generated, extends two objects") {
    val (ProtocolDefinitions(List(_, _, request: ClassDefinition[ScalaLanguage], requestFields: ClassDefinition[ScalaLanguage], _), _, _, _), _, _) =
      runSwaggerSpec(swagger)(Context.empty, Http4s)

    val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

    val expectedRequestTpe = t"""Request2"""

    val expectedRequestCls = q"""case class Request2(state2: Option[BigInt] = None, id: Option[String] = None, id2: String)"""

    val expectedRequestEncoder =
      q"""
        implicit val encodeRequest2: Encoder.AsObject[Request2] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[Request2](a => JsonObject.fromIterable(Vector(("state2", a.state2.asJson), ("id", a.id.asJson), ("id2", a.id2.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
      """
    val expectedRequestDecoder =
      q"""
        implicit val decodeRequest2: Decoder[Request2] = new Decoder[Request2] { final def apply(c: HCursor): Decoder.Result[Request2] = for (v0 <- c.downField("state2").as[Option[BigInt]]; v1 <- c.downField("id").as[Option[String]]; v2 <- c.downField("id2").as[String]) yield Request2(v0, v1, v2) }
      """

    compare(request.tpe, expectedRequestTpe)
    compare(request.cls, expectedRequestCls)
    compare(reqEncoder, expectedRequestEncoder)
    compare(reqDecoder, expectedRequestDecoder)

    val expectedFieldsTpe = t"""RequestFields2"""
    val expectedFieldsCls = q"""case class RequestFields2(state2: Option[BigInt] = None)"""

    val List(fieldsEncoder, fieldsDecoder) = requestFields.staticDefns.definitions

    val expectedFieldsEncoder =
      q"""
        implicit val encodeRequestFields2: Encoder.AsObject[RequestFields2] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[RequestFields2](a => JsonObject.fromIterable(Vector(("state2", a.state2.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
      """
    val expectedFieldsDecoder =
      q"""
        implicit val decodeRequestFields2: Decoder[RequestFields2] = new Decoder[RequestFields2] { final def apply(c: HCursor): Decoder.Result[RequestFields2] = for (v0 <- c.downField("state2").as[Option[BigInt]]) yield RequestFields2(v0) }
      """

    compare(requestFields.tpe, expectedFieldsTpe)
    compare(requestFields.cls, expectedFieldsCls)
    compare(fieldsEncoder, expectedFieldsEncoder)
    compare(fieldsDecoder, expectedFieldsDecoder)
  }

  test("Ensure case-to-case inheritance is not generated, extends two objects and two classes") {
    val (ProtocolDefinitions(List(_, _, _, _, request: ClassDefinition[ScalaLanguage]), _, _, _), _, _) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

    val expectedRequestTpe = t"""Request3"""

    val expectedRequestCls = q"""case class Request3(state: BigInt, state2: Option[BigInt] = None, id: Option[String] = None, id2: String)"""

    val expectedRequestEncoder =
      q"""
        implicit val encodeRequest3: Encoder.AsObject[Request3] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[Request3](a => JsonObject.fromIterable(Vector(("state", a.state.asJson), ("state2", a.state2.asJson), ("id", a.id.asJson), ("id2", a.id2.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
      """
    val expectedRequestDecoder =
      q"""
        implicit val decodeRequest3: Decoder[Request3] = new Decoder[Request3] { final def apply(c: HCursor): Decoder.Result[Request3] = for (v0 <- c.downField("state").as[BigInt]; v1 <- c.downField("state2").as[Option[BigInt]]; v2 <- c.downField("id").as[Option[String]]; v3 <- c.downField("id2").as[String]) yield Request3(v0, v1, v2, v3) }
      """

    compare(request.tpe, expectedRequestTpe)
    compare(request.cls, expectedRequestCls)
    compare(reqEncoder, expectedRequestEncoder)
    compare(reqDecoder, expectedRequestDecoder)

  }

  private def compare(t1: Tree, t2: Tree)(implicit pos: org.scalactic.source.Position): Assertion =
    t1.structure shouldEqual t2.structure
}
