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
       |    description: Request fields with id
       |    allOf:
       |      - "$$ref": "#/definitions/RequestFields"
       |      - type: object
       |        properties:
       |          id:
       |            type: string
       |  RequestFields:
       |    description: Request fields
       |    type: object
       |    properties:
       |      state:
       |        type: integer
       |    required: [state]    
       |  Request2:
       |    description: Request fields with id
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
       |    description: Request fields
       |    type: object
       |    properties:
       |      state2:
       |        type: integer
       |  Request3:
       |    description: Request fields with id
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
         implicit val encodeRequest: ObjectEncoder[Request] = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct2("state", "id")((o: Request) => (o.state, o.id)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key))) 
         }
      """
    val expectedRequestDecoder =
      q"""
         implicit val decodeRequest: Decoder[Request] = Decoder.forProduct2("state", "id")(Request.apply _)
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
         implicit val encodeRequestFields: ObjectEncoder[RequestFields] = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct1("state")((o: RequestFields) => o.state).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
         }
      """
    val expectedFieldsDecoder =
      q"""
         implicit val decodeRequestFields: Decoder[RequestFields] = Decoder.forProduct1("state")(RequestFields.apply _)
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
         implicit val encodeRequest2: ObjectEncoder[Request2] = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct3("state2", "id", "id2")((o: Request2) => (o.state2, o.id, o.id2)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key))) 
         }
      """
    val expectedRequestDecoder =
      q"""
         implicit val decodeRequest2: Decoder[Request2] = Decoder.forProduct3("state2", "id", "id2")(Request2.apply _)
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
         implicit val encodeRequestFields2: ObjectEncoder[RequestFields2] = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct1("state2")((o: RequestFields2) => o.state2).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
         }
      """
    val expectedFieldsDecoder =
      q"""
         implicit val decodeRequestFields2: Decoder[RequestFields2] = Decoder.forProduct1("state2")(RequestFields2.apply _)
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
         implicit val encodeRequest3: ObjectEncoder[Request3] = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct4("state", "state2", "id", "id2")((o: Request3) => (o.state, o.state2, o.id, o.id2)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key))) 
         }
      """
    val expectedRequestDecoder =
      q"""
         implicit val decodeRequest3: Decoder[Request3] = Decoder.forProduct4("state", "state2", "id", "id2")(Request3.apply _)
      """

    compare(request.tpe, expectedRequestTpe)
    compare(request.cls, expectedRequestCls)
    compare(reqEncoder, expectedRequestEncoder)
    compare(reqDecoder, expectedRequestDecoder)

  }

  private def compare(t1: Tree, t2: Tree)(implicit pos: org.scalactic.source.Position): Assertion =
    t1.structure shouldEqual t2.structure
}
