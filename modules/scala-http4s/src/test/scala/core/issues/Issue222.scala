package core.issues

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.http4s.Http4sVersion
import dev.guardrail.terms.protocol.ClassDefinition
import support.SwaggerSpecRunner

class Issue222 extends AnyFunSuite with Matchers with SwaggerSpecRunner {

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

  def testVersion(version: Http4sVersion): Unit = {
    test(s"$version - Ensure case-to-case inheritance is not generated") {
      val (x @ ProtocolDefinitions(List(request: ClassDefinition[ScalaLanguage], requestFields: ClassDefinition[ScalaLanguage], _, _, _), _, _, _, _), _, _) =
        runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

      val expectedRequestTpe = t"""Request"""

      val expectedRequestCls = q"""case class Request(state: BigInt, id: Option[String] = None)"""

      val expectedRequestEncoder =
        q"""
        implicit val encodeRequest: _root_.io.circe.Encoder.AsObject[Request] = {
          _root_.io.circe.Encoder.AsObject.instance[Request](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("state", a.state.asJson), ("id", a.id.asJson))))
        }
      """
      val expectedRequestDecoder =
        q"""
        implicit val decodeRequest: _root_.io.circe.Decoder[Request] = new _root_.io.circe.Decoder[Request] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Request] = for (v0 <- c.downField("state").as[BigInt]; v1 <- c.downField("id").as[Option[String]]) yield Request(v0, v1) }
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
        implicit val encodeRequestFields: _root_.io.circe.Encoder.AsObject[RequestFields] = {
          _root_.io.circe.Encoder.AsObject.instance[RequestFields](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("state", a.state.asJson))))
        }
      """
      val expectedFieldsDecoder =
        q"""
        implicit val decodeRequestFields: _root_.io.circe.Decoder[RequestFields] = new _root_.io.circe.Decoder[RequestFields] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[RequestFields] = for (v0 <- c.downField("state").as[BigInt]) yield RequestFields(v0) }
      """

      compare(requestFields.tpe, expectedFieldsTpe)
      compare(requestFields.cls, expectedFieldsCls)
      compare(fieldsEncoder, expectedFieldsEncoder)
      compare(fieldsDecoder, expectedFieldsDecoder)
    }

    test(s"$version - Ensure case-to-case inheritance is not generated, extends two objects") {
      val (ProtocolDefinitions(List(_, _, request: ClassDefinition[ScalaLanguage], requestFields: ClassDefinition[ScalaLanguage], _), _, _, _, _), _, _) =
        runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

      val expectedRequestTpe = t"""Request2"""

      val expectedRequestCls = q"""case class Request2(state2: Option[BigInt] = None, id: Option[String] = None, id2: String)"""

      val expectedRequestEncoder =
        q"""
        implicit val encodeRequest2: _root_.io.circe.Encoder.AsObject[Request2] = {
          _root_.io.circe.Encoder.AsObject.instance[Request2](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("state2", a.state2.asJson), ("id", a.id.asJson), ("id2", a.id2.asJson))))
        }
      """
      val expectedRequestDecoder =
        q"""
        implicit val decodeRequest2: _root_.io.circe.Decoder[Request2] = new _root_.io.circe.Decoder[Request2] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Request2] = for (v0 <- c.downField("state2").as[Option[BigInt]]; v1 <- c.downField("id").as[Option[String]]; v2 <- c.downField("id2").as[String]) yield Request2(v0, v1, v2) }
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
        implicit val encodeRequestFields2: _root_.io.circe.Encoder.AsObject[RequestFields2] = {
          _root_.io.circe.Encoder.AsObject.instance[RequestFields2](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("state2", a.state2.asJson))))
        }
      """
      val expectedFieldsDecoder =
        q"""
        implicit val decodeRequestFields2: _root_.io.circe.Decoder[RequestFields2] = new _root_.io.circe.Decoder[RequestFields2] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[RequestFields2] = for (v0 <- c.downField("state2").as[Option[BigInt]]) yield RequestFields2(v0) }
      """

      compare(requestFields.tpe, expectedFieldsTpe)
      compare(requestFields.cls, expectedFieldsCls)
      compare(fieldsEncoder, expectedFieldsEncoder)
      compare(fieldsDecoder, expectedFieldsDecoder)
    }

    test(s"$version - Ensure case-to-case inheritance is not generated, extends two objects and two classes") {
      val (ProtocolDefinitions(List(_, _, _, _, request: ClassDefinition[ScalaLanguage]), _, _, _, _), _, _) =
        runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, version.value)

      val List(reqEncoder, reqDecoder) = request.staticDefns.definitions

      val expectedRequestTpe = t"""Request3"""

      val expectedRequestCls = q"""case class Request3(state: BigInt, state2: Option[BigInt] = None, id: Option[String] = None, id2: String)"""

      val expectedRequestEncoder =
        q"""
        implicit val encodeRequest3: _root_.io.circe.Encoder.AsObject[Request3] = {
          _root_.io.circe.Encoder.AsObject.instance[Request3](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("state", a.state.asJson), ("state2", a.state2.asJson), ("id", a.id.asJson), ("id2", a.id2.asJson))))
        }
      """
      val expectedRequestDecoder =
        q"""
        implicit val decodeRequest3: _root_.io.circe.Decoder[Request3] = new _root_.io.circe.Decoder[Request3] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Request3] = for (v0 <- c.downField("state").as[BigInt]; v1 <- c.downField("state2").as[Option[BigInt]]; v2 <- c.downField("id").as[Option[String]]; v3 <- c.downField("id2").as[String]) yield Request3(v0, v1, v2, v3) }
      """

      compare(request.tpe, expectedRequestTpe)
      compare(request.cls, expectedRequestCls)
      compare(reqEncoder, expectedRequestEncoder)
      compare(reqDecoder, expectedRequestDecoder)

    }
  }

  private def compare(t1: Tree, t2: Tree)(implicit pos: org.scalactic.source.Position): Assertion =
    t1.structure shouldEqual t2.structure

  testVersion(Http4sVersion.V0_22)
  testVersion(Http4sVersion.V0_23)
}
