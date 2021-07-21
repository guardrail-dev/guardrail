package tests.generators.akkaHttp

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Context, Server, Servers }
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AkkaHttpServerTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  import scala.meta._

  val swagger: String = s"""
    |swagger: '2.0'
    |host: petstore.swagger.io
    |paths:
    |  /:
    |    get:
    |      x-jvm-package: store
    |      operationId: getRoot
    |      responses:
    |        200:
    |          description: Successful
    |  "/store/order/{order_id}":
    |    get:
    |      tags:
    |      - store
    |      x-jvm-package: store
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
    |      x-jvm-package: store
    |      operationId: getFoo
    |      responses:
    |        200:
    |          schema:
    |            type: boolean
    |  "/foo/{bar}":
    |    get:
    |      x-jvm-package: store
    |      x-tracing-label: "completely-custom-label"
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
    |      x-jvm-package: store
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
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val handler  = q"""
      trait StoreHandler {
        def getRoot(respond: StoreResource.GetRootResponse.type)(): scala.concurrent.Future[StoreResource.GetRootResponse]
        def putBar(respond: StoreResource.PutBarResponse.type)(bar: Long): scala.concurrent.Future[HttpResponse]
        def getFoo(respond: StoreResource.GetFooResponse.type)(): scala.concurrent.Future[StoreResource.GetFooResponse]
        def getFooBar(respond: StoreResource.GetFooBarResponse.type)(bar: Long): scala.concurrent.Future[StoreResource.GetFooBarResponse]
        def getOrderById(respond: StoreResource.GetOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed): scala.concurrent.Future[StoreResource.GetOrderByIdResponse]
      }
    """
    val resource = q"""
      object StoreResource {
        def routes(handler: StoreHandler)(implicit mat: akka.stream.Materializer): Route = {
          {
            pathEndOrSingleSlash(get(discardEntity(complete(handler.getRoot(GetRootResponse)()))))
          } ~ {
            path("bar")(put(parameter(Symbol("bar").as[Long]).apply(bar => discardEntity(complete(handler.putBar(PutBarResponse)(bar))))))
          } ~ {
            (pathPrefix("foo") & pathEndOrSingleSlash)(get(discardEntity(complete(handler.getFoo(GetFooResponse)()))))
          } ~ {
            path("foo" / LongNumber).apply(bar => get(discardEntity(complete(handler.getFooBar(GetFooBarResponse)(bar)))))
          } ~ {
            path("store" / "order" / LongNumber).apply(orderId => get(parameter(Symbol("status").as[OrderStatus](stringyJsonUnmarshaller.andThen(unmarshallJson[OrderStatus]))).apply(status => discardEntity(complete(handler.getOrderById(GetOrderByIdResponse)(orderId, status))))))
          }
        }
        sealed abstract class GetRootResponse(val statusCode: StatusCode)
        case object GetRootResponseOK extends GetRootResponse(StatusCodes.OK)
        object GetRootResponse {
          implicit def getRootResponseTRM: ToResponseMarshaller[GetRootResponse] = Marshaller { implicit ec =>
            resp => getRootResponseTR(resp)
          }
          implicit def getRootResponseTR(value: GetRootResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: GetRootResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetRootResponse): GetRootResponse = ev(value)
          def OK: GetRootResponse = GetRootResponseOK
        }
        sealed abstract class PutBarResponse(val statusCode: StatusCode)
        case class PutBarResponseOK(value: Boolean) extends PutBarResponse(StatusCodes.OK)
        object PutBarResponse {
          implicit def putBarResponseTRM: ToResponseMarshaller[PutBarResponse] = Marshaller { implicit ec =>
            resp => putBarResponseTR(resp)
          }
          implicit def putBarResponseTR(value: PutBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ PutBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => PutBarResponse): PutBarResponse = ev(value)
          implicit def OKEv(value: Boolean): PutBarResponse = OK(value)
          def OK(value: Boolean): PutBarResponse = PutBarResponseOK(value)
        }
        sealed abstract class GetFooResponse(val statusCode: StatusCode)
        case class GetFooResponseOK(value: Boolean) extends GetFooResponse(StatusCodes.OK)
        object GetFooResponse {
          implicit def getFooResponseTRM: ToResponseMarshaller[GetFooResponse] = Marshaller { implicit ec =>
            resp => getFooResponseTR(resp)
          }
          implicit def getFooResponseTR(value: GetFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetFooResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => GetFooResponse): GetFooResponse = ev(value)
          implicit def OKEv(value: Boolean): GetFooResponse = OK(value)
          def OK(value: Boolean): GetFooResponse = GetFooResponseOK(value)
        }
        sealed abstract class GetFooBarResponse(val statusCode: StatusCode)
        case class GetFooBarResponseOK(value: Boolean) extends GetFooBarResponse(StatusCodes.OK)
        object GetFooBarResponse {
          implicit def getFooBarResponseTRM: ToResponseMarshaller[GetFooBarResponse] = Marshaller { implicit ec =>
            resp => getFooBarResponseTR(resp)
          }
          implicit def getFooBarResponseTR(value: GetFooBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetFooBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => GetFooBarResponse): GetFooBarResponse = ev(value)
          implicit def OKEv(value: Boolean): GetFooBarResponse = OK(value)
          def OK(value: Boolean): GetFooBarResponse = GetFooBarResponseOK(value)
        }
        sealed abstract class GetOrderByIdResponse(val statusCode: StatusCode)
        case class GetOrderByIdResponseOK(value: Order) extends GetOrderByIdResponse(StatusCodes.OK)
        case object GetOrderByIdResponseBadRequest extends GetOrderByIdResponse(StatusCodes.BadRequest)
        case object GetOrderByIdResponseNotFound extends GetOrderByIdResponse(StatusCodes.NotFound)
        object GetOrderByIdResponse {
          implicit def getOrderByIdResponseTRM: ToResponseMarshaller[GetOrderByIdResponse] = Marshaller { implicit ec =>
            resp => getOrderByIdResponseTR(resp)
          }
          implicit def getOrderByIdResponseTR(value: GetOrderByIdResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetOrderByIdResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
            case r: GetOrderByIdResponseBadRequest.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
            case r: GetOrderByIdResponseNotFound.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetOrderByIdResponse): GetOrderByIdResponse = ev(value)
          implicit def OKEv(value: Order): GetOrderByIdResponse = OK(value)
          def OK(value: Order): GetOrderByIdResponse = GetOrderByIdResponseOK(value)
          def BadRequest: GetOrderByIdResponse = GetOrderByIdResponseBadRequest
          def NotFound: GetOrderByIdResponse = GetOrderByIdResponseNotFound
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
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty.copy(tracing = true), AkkaHttp)

    val handler  = q"""
      trait StoreHandler {
        def getRoot(respond: StoreResource.GetRootResponse.type)()(traceBuilder: TraceBuilder): scala.concurrent.Future[StoreResource.GetRootResponse]
        def putBar(respond: StoreResource.PutBarResponse.type)(bar: Long)(traceBuilder: TraceBuilder): scala.concurrent.Future[HttpResponse]
        def getFoo(respond: StoreResource.GetFooResponse.type)()(traceBuilder: TraceBuilder): scala.concurrent.Future[StoreResource.GetFooResponse]
        def getFooBar(respond: StoreResource.GetFooBarResponse.type)(bar: Long)(traceBuilder: TraceBuilder): scala.concurrent.Future[StoreResource.GetFooBarResponse]
        def getOrderById(respond: StoreResource.GetOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed)(traceBuilder: TraceBuilder): scala.concurrent.Future[StoreResource.GetOrderByIdResponse]
      }
    """
    val resource = q"""
      object StoreResource {
        def routes(handler: StoreHandler, trace: String => Directive1[TraceBuilder])(implicit mat: akka.stream.Materializer): Route = {
          {
            pathEndOrSingleSlash(get(trace("store:getRoot").apply(traceBuilder => discardEntity(complete(handler.getRoot(GetRootResponse)()(traceBuilder))))))
          } ~ {
            path("bar")(put(parameter(Symbol("bar").as[Long]).apply(bar => trace("store:putBar").apply(traceBuilder => discardEntity(complete(handler.putBar(PutBarResponse)(bar)(traceBuilder)))))))
          } ~ {
            (pathPrefix("foo") & pathEndOrSingleSlash)(get(trace("store:getFoo").apply(traceBuilder => discardEntity(complete(handler.getFoo(GetFooResponse)()(traceBuilder))))))
          } ~ {
            path("foo" / LongNumber).apply(bar => get(trace("completely-custom-label").apply(traceBuilder => discardEntity(complete(handler.getFooBar(GetFooBarResponse)(bar)(traceBuilder))))))
          } ~ {
            path("store" / "order" / LongNumber).apply(orderId => get(parameter(Symbol("status").as[OrderStatus](stringyJsonUnmarshaller.andThen(unmarshallJson[OrderStatus]))).apply(status => trace("store:getOrderById").apply(traceBuilder => discardEntity(complete(handler.getOrderById(GetOrderByIdResponse)(orderId, status)(traceBuilder)))))))
          }
        }
        sealed abstract class GetRootResponse(val statusCode: StatusCode)
        case object GetRootResponseOK extends GetRootResponse(StatusCodes.OK)
        object GetRootResponse {
          implicit def getRootResponseTRM: ToResponseMarshaller[GetRootResponse] = Marshaller { implicit ec =>
            resp => getRootResponseTR(resp)
          }
          implicit def getRootResponseTR(value: GetRootResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: GetRootResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetRootResponse): GetRootResponse = ev(value)
          def OK: GetRootResponse = GetRootResponseOK
        }
        sealed abstract class PutBarResponse(val statusCode: StatusCode)
        case class PutBarResponseOK(value: Boolean) extends PutBarResponse(StatusCodes.OK)
        object PutBarResponse {
          implicit def putBarResponseTRM: ToResponseMarshaller[PutBarResponse] = Marshaller { implicit ec =>
            resp => putBarResponseTR(resp)
          }
          implicit def putBarResponseTR(value: PutBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ PutBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => PutBarResponse): PutBarResponse = ev(value)
          implicit def OKEv(value: Boolean): PutBarResponse = OK(value)
          def OK(value: Boolean): PutBarResponse = PutBarResponseOK(value)
        }
        sealed abstract class GetFooResponse(val statusCode: StatusCode)
        case class GetFooResponseOK(value: Boolean) extends GetFooResponse(StatusCodes.OK)
        object GetFooResponse {
          implicit def getFooResponseTRM: ToResponseMarshaller[GetFooResponse] = Marshaller { implicit ec =>
            resp => getFooResponseTR(resp)
          }
          implicit def getFooResponseTR(value: GetFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetFooResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => GetFooResponse): GetFooResponse = ev(value)
          implicit def OKEv(value: Boolean): GetFooResponse = OK(value)
          def OK(value: Boolean): GetFooResponse = GetFooResponseOK(value)
        }
        sealed abstract class GetFooBarResponse(val statusCode: StatusCode)
        case class GetFooBarResponseOK(value: Boolean) extends GetFooBarResponse(StatusCodes.OK)
        object GetFooBarResponse {
          implicit def getFooBarResponseTRM: ToResponseMarshaller[GetFooBarResponse] = Marshaller { implicit ec =>
            resp => getFooBarResponseTR(resp)
          }
          implicit def getFooBarResponseTR(value: GetFooBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetFooBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => GetFooBarResponse): GetFooBarResponse = ev(value)
          implicit def OKEv(value: Boolean): GetFooBarResponse = OK(value)
          def OK(value: Boolean): GetFooBarResponse = GetFooBarResponseOK(value)
        }
        sealed abstract class GetOrderByIdResponse(val statusCode: StatusCode)
        case class GetOrderByIdResponseOK(value: Order) extends GetOrderByIdResponse(StatusCodes.OK)
        case object GetOrderByIdResponseBadRequest extends GetOrderByIdResponse(StatusCodes.BadRequest)
        case object GetOrderByIdResponseNotFound extends GetOrderByIdResponse(StatusCodes.NotFound)
        object GetOrderByIdResponse {
          implicit def getOrderByIdResponseTRM: ToResponseMarshaller[GetOrderByIdResponse] = Marshaller { implicit ec =>
            resp => getOrderByIdResponseTR(resp)
          }
          implicit def getOrderByIdResponseTR(value: GetOrderByIdResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetOrderByIdResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
            case r: GetOrderByIdResponseBadRequest.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
            case r: GetOrderByIdResponseNotFound.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetOrderByIdResponse): GetOrderByIdResponse = ev(value)
          implicit def OKEv(value: Order): GetOrderByIdResponse = OK(value)
          def OK(value: Order): GetOrderByIdResponse = GetOrderByIdResponseOK(value)
          def BadRequest: GetOrderByIdResponse = GetOrderByIdResponseBadRequest
          def NotFound: GetOrderByIdResponse = GetOrderByIdResponseNotFound
        }
      }
    """

    genHandler.structure should equal(handler.structure)
    genResource.structure should equal(resource.structure)
  }

  test("Ensure routes are generated with custom extraction") {
    val (
      _,
      _,
      Servers(Server(pkg, extraImports, genHandler, genResource :: Nil) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty.copy(customExtraction = true), AkkaHttp)

    val handler  = q"""
      trait StoreHandler[-E] {
        def getRoot(respond: StoreResource.GetRootResponse.type)()(extracted: E): scala.concurrent.Future[StoreResource.GetRootResponse]
        def putBar(respond: StoreResource.PutBarResponse.type)(bar: Long)(extracted: E): scala.concurrent.Future[HttpResponse]
        def getFoo(respond: StoreResource.GetFooResponse.type)()(extracted: E): scala.concurrent.Future[StoreResource.GetFooResponse]
        def getFooBar(respond: StoreResource.GetFooBarResponse.type)(bar: Long)(extracted: E): scala.concurrent.Future[StoreResource.GetFooBarResponse]
        def getOrderById(respond: StoreResource.GetOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed)(extracted: E): scala.concurrent.Future[StoreResource.GetOrderByIdResponse]
      }
    """
    val resource = q"""
      object StoreResource {
        def routes[E](handler: StoreHandler[E], customExtract: String => Directive1[E])(implicit mat: akka.stream.Materializer): Route = {
          {
            pathEndOrSingleSlash(get(customExtract("getRoot").apply(extracted => discardEntity(complete(handler.getRoot(GetRootResponse)()(extracted))))))
          } ~ ({
            path("bar")(put(customExtract("putBar").apply(extracted => parameter(Symbol("bar").as[Long]).apply(bar => discardEntity(complete(handler.putBar(PutBarResponse)(bar)(extracted)))))))
          }) ~ ({
            (pathPrefix("foo") & pathEndOrSingleSlash)(get(customExtract("getFoo").apply(extracted => discardEntity(complete(handler.getFoo(GetFooResponse)()(extracted))))))
          }) ~ ({
            path("foo" / LongNumber).apply(bar => get(customExtract("getFooBar").apply(extracted => discardEntity(complete(handler.getFooBar(GetFooBarResponse)(bar)(extracted))))))
          }) ~ ({
            path("store" / "order" / LongNumber).apply(orderId => get(customExtract("getOrderById").apply(extracted => parameter(Symbol("status").as[OrderStatus](stringyJsonUnmarshaller.andThen(unmarshallJson[OrderStatus]))).apply(status => discardEntity(complete(handler.getOrderById(GetOrderByIdResponse)(orderId, status)(extracted)))))))
          })
        }
        sealed abstract class GetRootResponse(val statusCode: StatusCode)
        case object GetRootResponseOK extends GetRootResponse(StatusCodes.OK)
        object GetRootResponse {
          implicit def getRootResponseTRM: ToResponseMarshaller[GetRootResponse] = Marshaller { implicit ec =>
            resp => getRootResponseTR(resp)
          }
          implicit def getRootResponseTR(value: GetRootResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r: GetRootResponseOK.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetRootResponse): GetRootResponse = ev(value)
          def OK: GetRootResponse = GetRootResponseOK
        }
        sealed abstract class PutBarResponse(val statusCode: StatusCode)
        case class PutBarResponseOK(value: Boolean) extends PutBarResponse(StatusCodes.OK)
        object PutBarResponse {
          implicit def putBarResponseTRM: ToResponseMarshaller[PutBarResponse] = Marshaller { implicit ec =>
            resp => putBarResponseTR(resp)
          }
          implicit def putBarResponseTR(value: PutBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ PutBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => PutBarResponse): PutBarResponse = ev(value)
          implicit def OKEv(value: Boolean): PutBarResponse = OK(value)
          def OK(value: Boolean): PutBarResponse = PutBarResponseOK(value)
        }
        sealed abstract class GetFooResponse(val statusCode: StatusCode)
        case class GetFooResponseOK(value: Boolean) extends GetFooResponse(StatusCodes.OK)
        object GetFooResponse {
          implicit def getFooResponseTRM: ToResponseMarshaller[GetFooResponse] = Marshaller { implicit ec =>
            resp => getFooResponseTR(resp)
          }
          implicit def getFooResponseTR(value: GetFooResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetFooResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => GetFooResponse): GetFooResponse = ev(value)
          implicit def OKEv(value: Boolean): GetFooResponse = OK(value)
          def OK(value: Boolean): GetFooResponse = GetFooResponseOK(value)
        }
        sealed abstract class GetFooBarResponse(val statusCode: StatusCode)
        case class GetFooBarResponseOK(value: Boolean) extends GetFooBarResponse(StatusCodes.OK)
        object GetFooBarResponse {
          implicit def getFooBarResponseTRM: ToResponseMarshaller[GetFooBarResponse] = Marshaller { implicit ec =>
            resp => getFooBarResponseTR(resp)
          }
          implicit def getFooBarResponseTR(value: GetFooBarResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetFooBarResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
          }
          def apply[T](value: T)(implicit ev: T => GetFooBarResponse): GetFooBarResponse = ev(value)
          implicit def OKEv(value: Boolean): GetFooBarResponse = OK(value)
          def OK(value: Boolean): GetFooBarResponse = GetFooBarResponseOK(value)
        }
        sealed abstract class GetOrderByIdResponse(val statusCode: StatusCode)
        case class GetOrderByIdResponseOK(value: Order) extends GetOrderByIdResponse(StatusCodes.OK)
        case object GetOrderByIdResponseBadRequest extends GetOrderByIdResponse(StatusCodes.BadRequest)
        case object GetOrderByIdResponseNotFound extends GetOrderByIdResponse(StatusCodes.NotFound)
        object GetOrderByIdResponse {
          implicit def getOrderByIdResponseTRM: ToResponseMarshaller[GetOrderByIdResponse] = Marshaller { implicit ec =>
            resp => getOrderByIdResponseTR(resp)
          }
          implicit def getOrderByIdResponseTR(value: GetOrderByIdResponse)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[List[Marshalling[HttpResponse]]] = value match {
            case r @ GetOrderByIdResponseOK(value) =>
              Marshal(value).to[ResponseEntity].map {
                entity => Marshalling.Opaque {
                  () => HttpResponse(r.statusCode, entity = entity)
                } :: Nil
              }
            case r: GetOrderByIdResponseBadRequest.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
            case r: GetOrderByIdResponseNotFound.type =>
              scala.concurrent.Future.successful(Marshalling.Opaque {
                () => HttpResponse(r.statusCode)
              } :: Nil)
          }
          def apply[T](value: T)(implicit ev: T => GetOrderByIdResponse): GetOrderByIdResponse = ev(value)
          implicit def OKEv(value: Order): GetOrderByIdResponse = OK(value)
          def OK(value: Order): GetOrderByIdResponse = GetOrderByIdResponseOK(value)
          def BadRequest: GetOrderByIdResponse = GetOrderByIdResponseBadRequest
          def NotFound: GetOrderByIdResponse = GetOrderByIdResponseNotFound
        }
      }
    """

    genHandler.structure should equal(handler.structure)
    genResource.structure should equal(resource.structure)
  }
}
