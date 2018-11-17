package tests.generators.http4s.client

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{ Client, Clients, Context }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

class DefaultParametersTest extends FunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |paths:
    |  "/store/order/{order_id}":
    |    get:
    |      tags:
    |      - store
    |      x-scala-package: store
    |      operationId: getOrderById
    |      produces:
    |      - application/xml
    |      - application/json
    |      parameters:
    |      - name: order_id
    |        in: path
    |        required: true
    |        type: integer
    |        maximum: 10
    |        minimum: 1
    |        format: int64
    |      - name: HeaderMeThis
    |        in: header
    |        type: string
    |        required: true
    |      - name: defparm_opt
    |        in: query
    |        type: integer
    |        format: int32
    |        default: 1
    |      - name: defparm
    |        in: query
    |        type: integer
    |        format: int32
    |        required: true
    |        default: 2
    |      responses:
    |        '200':
    |          description: successful operation
    |          schema:
    |            "$$ref": "#/definitions/Order"
    |        '400':
    |          description: Invalid ID supplied
    |        '404':
    |          description: Order not found
    |    delete:
    |      tags:
    |      - store
    |      x-scala-package: store
    |      summary: Delete purchase order by ID
    |      description: For valid response try integer IDs with positive integer value.
    |        Negative or non-integer values will generate API errors
    |      operationId: deleteOrder
    |      produces:
    |      - application/xml
    |      - application/json
    |      parameters:
    |      - name: order_id
    |        in: path
    |        description: ID of the order that needs to be deleted
    |        required: true
    |        type: integer
    |        minimum: 1
    |        format: int64
    |      responses:
    |        '400':
    |          description: Invalid ID supplied
    |        '404':
    |          description: Order not found
    |securityDefinitions:
    |  petstore_auth:
    |    type: oauth2
    |    authorizationUrl: http://petstore.swagger.io/oauth/dialog
    |    flow: implicit
    |    scopes:
    |      write:pets: modify pets in your account
    |      read:pets: read your pets
    |  api_key:
    |    type: apiKey
    |    name: api_key
    |    in: header
    |definitions:
    |  Order:
    |    type: object
    |    properties:
    |      id:
    |        type: integer
    |        format: int64
    |      petId:
    |        type: integer
    |        format: int64
    |      quantity:
    |        type: integer
    |        format: int32
    |      shipDate:
    |        type: string
    |        format: date-time
    |      status:
    |        type: string
    |        description: Order Status
    |        enum:
    |        - placed
    |        - approved
    |        - delivered
    |      complete:
    |        type: boolean
    |        default: false
    |    xml:
    |      name: Order
    |""".stripMargin

  test("Ensure responses are generated") {
    val (
      _,
      Clients(Client(tags, className, _, cls, cmp, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s, defaults.http4sGeneratorSettings)

    tags should equal(Seq("store"))

    val actual = cls +: cmp +: statements

    val expected = List(
      q"""object StoreClient {
      def apply[F[_]](host: String = "http://petstore.swagger.io")(implicit effect: Effect[F], httpClient: Http4sClient[F]): StoreClient[F] = new StoreClient[F](host = host)(effect = effect, httpClient = httpClient)
      def httpClient[F[_]](httpClient: Http4sClient[F], host: String = "http://petstore.swagger.io")(implicit effect: Effect[F]): StoreClient[F] = new StoreClient[F](host = host)(effect = effect, httpClient = httpClient)
    }""",
      q"""class StoreClient[F[_]](host: String = "http://petstore.swagger.io")(implicit effect: Effect[F], httpClient: Http4sClient[F]) {
      val basePath: String = ""
      val getOrderByIdOkDecoder = jsonOf[F, Order]
      def getOrderById(orderId: Long, defparmOpt: Option[Int] = Option(1), defparm: Int = 2, headerMeThis: String, headers: List[Header] = List.empty): F[GetOrderByIdResponse] = {
        val allHeaders = headers ++ List[Option[Header]](Some(Header("HeaderMeThis", Formatter.show(headerMeThis)))).flatten
        val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/store/order/" + Formatter.addPath(orderId) + "?" + Formatter.addArg("defparm_opt", defparmOpt) + Formatter.addArg("defparm", defparm)), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(resp) =>
            getOrderByIdOkDecoder.decode(resp, strict = false).fold(throw _, identity).map(GetOrderByIdResponse.Ok)
          case BadRequest(_) =>
            effect.pure(GetOrderByIdResponse.BadRequest)
          case NotFound(_) =>
            effect.pure(GetOrderByIdResponse.NotFound)
        })
      }
      def deleteOrder(orderId: Long, headers: List[Header] = List.empty): F[DeleteOrderResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.DELETE, uri = Uri.unsafeFromString(host + basePath + "/store/order/" + Formatter.addPath(orderId)), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case BadRequest(_) =>
            effect.pure(DeleteOrderResponse.BadRequest)
          case NotFound(_) =>
            effect.pure(DeleteOrderResponse.NotFound)
        })
      }
    }""",
      q"""sealed abstract class GetOrderByIdResponse""",
      q"""object GetOrderByIdResponse {
      case class Ok(value: Order) extends GetOrderByIdResponse
      case object BadRequest extends GetOrderByIdResponse
      case object NotFound extends GetOrderByIdResponse
    }""",
      q"""sealed abstract class DeleteOrderResponse""",
      q"""object DeleteOrderResponse {
      case object BadRequest extends DeleteOrderResponse
      case object NotFound extends DeleteOrderResponse
    }"""
    )

    actual.map(_.structure) should equal(expected.map(_.structure))
  }
}
