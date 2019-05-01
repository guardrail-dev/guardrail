package core.issues

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.{ClassDefinition, Context, ProtocolDefinitions}
import org.scalatest.{Assertion, FunSuite, Matchers}
import support.SwaggerSpecRunner

class Issue222 extends FunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
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
                           |  Request2:
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
                           |  RequestFields2:
                           |    description: Request fields
                           |    type: object
                           |    properties:
                           |      state:
                           |        type: integer
                           |        
                           |""".stripMargin

  test("Ensure case-to-case inheritance is not generated") {
    val (ProtocolDefinitions(List(request: ClassDefinition[ScalaLanguage], requestFields: ClassDefinition[ScalaLanguage], _, _), _, _, _), _, _) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

    val expectedRequestTpe = t"""Request"""

    val expectedRequestCls = q"""case class Request(state: Option[BigInt] = None, id: Option[String] = None)"""

    val expectedRequestEncoder = q"""
         implicit val encodeRequest = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct2("state", "id")((o: Request) => (o.state, o.id)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key))) 
         }
      """
    val expectedRequestDecoder = q"""
         implicit val decodeRequest = Decoder.forProduct2("state", "id")(Request.apply _)
      """


    compare(request.tpe, expectedRequestTpe)
    compare(request.cls, expectedRequestCls)
    compare(reqEncoder, expectedRequestEncoder)
    compare(reqDecoder, expectedRequestDecoder)

    val expectedFieldsTpe = t"""RequestFields"""
    val expectedFieldsCls = q"""case class RequestFields(state: Option[BigInt] = None)"""

    val List(fieldsEncoder, fieldsDecoder) = requestFields.staticDefns.definitions

    val expectedFieldsEncoder =  q"""
         implicit val encodeRequestFields = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct1("state")((o: RequestFields) => o.state).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
         }
      """
    val expectedFieldsDecoder = q"""
         implicit val decodeRequestFields = Decoder.forProduct1("state")(RequestFields.apply _)
      """

    compare(requestFields.tpe, expectedFieldsTpe)
    compare(requestFields.cls, expectedFieldsCls)
    compare(fieldsEncoder, expectedFieldsEncoder)
    compare(fieldsDecoder, expectedFieldsDecoder)
  }

  test("Ensure case-to-case inheritance is not generated, more complicated scenario") {
    val (ProtocolDefinitions(List(_, _,request: ClassDefinition[ScalaLanguage], requestFields: ClassDefinition[ScalaLanguage]), _, _, _), _, _) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

    val expectedRequestTpe = t"""Request2"""

    val expectedRequestCls = q"""case class Request2(state: Option[BigInt] = None, id: Option[String] = None, id2: Option[String] = None)"""

    val expectedRequestEncoder = q"""
         implicit val encodeRequest2 = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct3("state", "id", "id2")((o: Request2) => (o.state, o.id, o.id2)).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key))) 
         }
      """
    val expectedRequestDecoder = q"""
         implicit val decodeRequest2 = Decoder.forProduct3("state", "id", "id2")(Request2.apply _)
      """


    compare(request.tpe, expectedRequestTpe)
    compare(request.cls, expectedRequestCls)
    compare(reqEncoder, expectedRequestEncoder)
    compare(reqDecoder, expectedRequestDecoder)

    val expectedFieldsTpe = t"""RequestFields2"""
    val expectedFieldsCls = q"""case class RequestFields2(state: Option[BigInt] = None)"""

    val List(fieldsEncoder, fieldsDecoder) = requestFields.staticDefns.definitions

    val expectedFieldsEncoder =  q"""
         implicit val encodeRequestFields2 = {
           val readOnlyKeys = Set[String]()
           Encoder.forProduct1("state")((o: RequestFields2) => o.state).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
         }
      """
    val expectedFieldsDecoder = q"""
         implicit val decodeRequestFields2 = Decoder.forProduct1("state")(RequestFields2.apply _)
      """

    compare(requestFields.tpe, expectedFieldsTpe)
    compare(requestFields.cls, expectedFieldsCls)
    compare(fieldsEncoder, expectedFieldsEncoder)
    compare(fieldsDecoder, expectedFieldsDecoder)
  }




  private def compare(t1: Tree, t2: Tree): Assertion = {
    println(s"expected: ${t1.syntax}")
    println(s"actual: ${t2.syntax}")
    t1.structure shouldEqual t2.structure
  }
}