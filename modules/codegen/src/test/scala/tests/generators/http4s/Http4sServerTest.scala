package tests.generators.http4s

import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.{Context, Server, Servers}
import org.scalatest.{FunSuite, Matchers}
import support.SwaggerSpecRunner

class Http4sServerTest extends FunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  "/":
       |    post:
       |      x-scala-package: store
       |      operationId: addOrder
       |      responses:
       |        200:
       |         description: description
       |      consumes:
       |        - application/json
       |      produces:
       |        - application/json
       |      parameters:
       |        - in: body
       |          name: body
       |          required: true
       |          schema:
       |            $$ref: '#/definitions/Order'
       |  "/store/order/{order_id}":
       |    get:
       |      tags:
       |       - store
       |      x-scala-package: store
       |      operationId: getOrderById
       |      produces:
       |      - application/xml
       |      - application/json
       |      parameters:
       |       - name: order_id
       |         in: path
       |         required: true
       |         type: integer
       |         format: int64
       |       - name: status
       |         in: query
       |         required: true
       |         type: string
       |         x-scala-type: OrderStatus
       |         default: placed
       |      responses:
       |        '200':
       |          description: successful operation
       |          schema:
       |            $$ref: "#/definitions/Order"
       |        '400':
       |          description: Invalid ID supplied
       |        '404':
       |          description: Order not found
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
       |  OrderStatus:
       |    type: string
       |    enum:
       |    - placed
       |    - approved
       |    - delivered
       |""".stripMargin

  test("Ensure routes are generated") {
    val (_, _, Servers(Server(_, _, genHandler :: genResource :: _) :: Nil)) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val handler = q"""
      trait StoreHandler[F[_]] {
        def addOrder(respond: AddOrderResponse.type)(body: Order): F[AddOrderResponse]
        def getOrderById(respond: GetOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed): F[GetOrderByIdResponse]
      }
    """
    val resource = q"""
      class StoreResource[F[_]]()(implicit E: Effect[F]) extends Http4sDsl[F] {

        val addOrderDecoder: EntityDecoder[F, Order] = jsonOf[F, Order]

        object GetOrderByIdStatusMatcher extends QueryParamDecoderMatcher[OrderStatus]("status")

        implicit val statusQueryParamDecoder: QueryParamDecoder[OrderStatus] = (value: QueryParameterValue) => Json.fromString(value.value).as[OrderStatus].leftMap(t => ParseFailure("Query decoding failed", t.getMessage)).toValidatedNel

        val getOrderByIdOkEncoder = jsonEncoderOf[F, Order]

        def routes(handler: StoreHandler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ POST -> Root =>
              req.decodeWith(addOrderDecoder, strict = false) { body =>
                handler.addOrder(AddOrderResponse)(body) flatMap {
                  case AddOrderResponse.Ok =>
                    Ok()
                }
              }
            case req @ GET -> Root / "store" / "order" / LongVar(orderId) :? GetOrderByIdStatusMatcher(status) =>
              handler.getOrderById(GetOrderByIdResponse)(orderId, status) flatMap {
                case GetOrderByIdResponse.Ok(value) =>
                  Ok(value)(E, getOrderByIdOkEncoder)
                case GetOrderByIdResponse.BadRequest =>
                  BadRequest()
                case GetOrderByIdResponse.NotFound =>
                  NotFound()
              }
          }
        }
      }
    """

    genHandler.structure shouldEqual handler.structure

    // Cause structure is slightly different but source code is the same the value converted to string and then parsed
    genResource.toString().parse[Stat].get.structure shouldEqual resource.structure
  }
}
