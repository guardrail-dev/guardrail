package tests.generators.akkaHttp

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Context, Server, Servers }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

class AkkaHttpServerTest extends FunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |paths:
    |  /:
    |    get:
    |      x-scala-package: store
    |      operationId: getRoot
    |      responses:
    |        200:
    |          description: Successful
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
    |        format: int64
    |      - name: status
    |        in: query
    |        required: true
    |        type: string
    |        x-scala-type: OrderStatus
    |        default: placed
    |      responses:
    |        '200':
    |          description: successful operation
    |          schema:
    |            "$$ref": "#/definitions/Order"
    |        '400':
    |          description: Invalid ID supplied
    |        '404':
    |          description: Order not found
    |  /foo/:
    |    get:
    |      x-scala-package: store
    |      operationId: getFoo
    |      responses:
    |        200:
    |          schema:
    |            type: boolean
    |  "/foo/{bar}":
    |    get:
    |      x-scala-package: store
    |      x-scala-tracing-label: "completely-custom-label"
    |      operationId: getFooBar
    |      parameters:
    |      - name: bar
    |        in: path
    |        required: true
    |        type: integer
    |        format: int64
    |      responses:
    |        200:
    |          schema:
    |            type: boolean
    |  "/bar":
    |    put:
    |      operationId: putBar
    |      x-server-raw-response: true
    |      x-scala-package: store
    |      parameters:
    |      - name: bar
    |        in: query
    |        required: true
    |        type: integer
    |        format: int64
    |      responses:
    |        200:
    |          schema:
    |            type: boolean
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
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler :: genResource :: Nil) :: Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val handler  = q"""
      trait StoreHandler {
        def getRoot(respond: StoreResource.getRootResponse.type)(): scala.concurrent.Future[StoreResource.getRootResponse]
        def getOrderById(respond: StoreResource.getOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed): scala.concurrent.Future[StoreResource.getOrderByIdResponse]
        def getFoo(respond: StoreResource.getFooResponse.type)(): scala.concurrent.Future[StoreResource.getFooResponse]
        def getFooBar(respond: StoreResource.getFooBarResponse.type)(bar: Long): scala.concurrent.Future[StoreResource.getFooBarResponse]
        def putBar(respond: StoreResource.putBarResponse.type)(bar: Long): scala.concurrent.Future[HttpResponse]
      }
    """
    val resource = q"""
      object StoreResource {
        def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap { req =>
          req.discardEntityBytes().future
          Directive.Empty
        }
        def routes(handler: StoreHandler)(implicit mat: akka.stream.Materializer): Route = {
          (get & pathEndOrSingleSlash & discardEntity) {
            complete(handler.getRoot(getRootResponse)())
          } ~ (get & path("store" / "order" / LongNumber) & parameter(Symbol("status").as[OrderStatus]) & discardEntity) {
            (orderId, status) => complete(handler.getOrderById(getOrderByIdResponse)(orderId, status))
          } ~ (get & (pathPrefix("foo") & pathEndOrSingleSlash) & discardEntity) {
            complete(handler.getFoo(getFooResponse)())
          } ~ (get & path("foo" / LongNumber) & discardEntity) {
            bar => complete(handler.getFooBar(getFooBarResponse)(bar))
          } ~ (put & path("bar") & parameter(Symbol("bar").as[Long]) & discardEntity) {
            bar => complete(handler.putBar(putBarResponse)(bar))
          }
        }
        sealed abstract class getRootResponse(val statusCode: StatusCode)
        case object getRootResponseOK extends getRootResponse(StatusCodes.OK)
        object getRootResponse {
          implicit val getRootTRM: ToResponseMarshaller[getRootResponse] = Marshaller { implicit ec =>
            resp => getRootTR(resp)
          }
          implicit def getRootTR(value: getRootResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: getRootResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => getRootResponse): getRootResponse = ev(value)
          def OK: getRootResponse = getRootResponseOK
        }
        sealed abstract class getOrderByIdResponse(val statusCode: StatusCode)
        case class getOrderByIdResponseOK(value: Order) extends getOrderByIdResponse(StatusCodes.OK)
        case object getOrderByIdResponseBadRequest extends getOrderByIdResponse(StatusCodes.BadRequest)
        case object getOrderByIdResponseNotFound extends getOrderByIdResponse(StatusCodes.NotFound)
        object getOrderByIdResponse {
          implicit val getOrderByIdTRM: ToResponseMarshaller[getOrderByIdResponse] = Marshaller { implicit ec =>
            resp => getOrderByIdTR(resp)
          }
          implicit def getOrderByIdTR(value: getOrderByIdResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ getOrderByIdResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
            case r: getOrderByIdResponseBadRequest.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
            case r: getOrderByIdResponseNotFound.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => getOrderByIdResponse): getOrderByIdResponse = ev(value)
          implicit def OKEv(value: Order): getOrderByIdResponse = OK(value)
          def OK(value: Order): getOrderByIdResponse = getOrderByIdResponseOK(value)
          def BadRequest: getOrderByIdResponse = getOrderByIdResponseBadRequest
          def NotFound: getOrderByIdResponse = getOrderByIdResponseNotFound
        }
        sealed abstract class getFooResponse(val statusCode: StatusCode)
        case class getFooResponseOK(value: Boolean) extends getFooResponse(StatusCodes.OK)
        object getFooResponse {
          implicit val getFooTRM: ToResponseMarshaller[getFooResponse] = Marshaller { implicit ec =>
            resp => getFooTR(resp)
          }
          implicit def getFooTR(value: getFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ getFooResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => getFooResponse): getFooResponse = ev(value)
          implicit def OKEv(value: Boolean): getFooResponse = OK(value)
          def OK(value: Boolean): getFooResponse = getFooResponseOK(value)
        }
        sealed abstract class getFooBarResponse(val statusCode: StatusCode)
        case class getFooBarResponseOK(value: Boolean) extends getFooBarResponse(StatusCodes.OK)
        object getFooBarResponse {
          implicit val getFooBarTRM: ToResponseMarshaller[getFooBarResponse] = Marshaller { implicit ec =>
            resp => getFooBarTR(resp)
          }
          implicit def getFooBarTR(value: getFooBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ getFooBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => getFooBarResponse): getFooBarResponse = ev(value)
          implicit def OKEv(value: Boolean): getFooBarResponse = OK(value)
          def OK(value: Boolean): getFooBarResponse = getFooBarResponseOK(value)
        }
        sealed abstract class putBarResponse(val statusCode: StatusCode)
        case class putBarResponseOK(value: Boolean) extends putBarResponse(StatusCodes.OK)
        object putBarResponse {
          implicit val putBarTRM: ToResponseMarshaller[putBarResponse] = Marshaller { implicit ec =>
            resp => putBarTR(resp)
          }
          implicit def putBarTR(value: putBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ putBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => putBarResponse): putBarResponse = ev(value)
          implicit def OKEv(value: Boolean): putBarResponse = OK(value)
          def OK(value: Boolean): putBarResponse = putBarResponseOK(value)
        }
      }
    """

    genHandler.structure shouldEqual handler.structure
    genResource.structure shouldEqual resource.structure
  }

  test("Ensure routes are generated with tracing") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler :: genResource :: Nil) :: Nil)
    ) = runSwaggerSpec(swagger)(Context.empty.copy(tracing = true), AkkaHttp, defaults.akkaGeneratorSettings)

    val handler  = q"""
      trait BazHandler {
        def getFoo(bar: Long)(implicit traceBuilder: TraceBuilder): scala.concurrent.Future[Boolean]
      }
    """
    val resource = q"""
      object BazResource {
        def discardEntity(implicit mat: akka.stream.Materializer): Directive0 = extractRequest.flatMap { req =>
          req.discardEntityBytes().future
          Directive.Empty
        }
        def routes(handler: BazHandler, trace: String => Directive1[TraceBuilder])(implicit mat: akka.stream.Materializer): Route = {
          (get & path("foo" / LongNumber) & discardEntity & trace("completely-custom-label")) { (bar, traceBuilder) =>
            complete(handler.getFoo(bar)(traceBuilder))
          }
        }
      }
    """

//    TODO: Unsupported for akka-http
//    genHandler.structure should equal(handler.structure)
//    genResource.structure should equal(resource.structure)
  }
}
