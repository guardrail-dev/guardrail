package tests.generators.http4s

import dev.guardrail.generators.Scala.Http4s
import dev.guardrail.{ Context, Server, Servers }
import support.SwaggerSpecRunner
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Http4sServerTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  import scala.meta._

  val swagger: String =
    s"""
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
      Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    val handler =
      q"""
      trait StoreHandler[F[_]] {
        def getFoo(respond: GetFooResponse.type)(): F[GetFooResponse]
        def getFooBar(respond: GetFooBarResponse.type)(bar: Long): F[GetFooBarResponse]
        def getOrderById(respond: GetOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed): F[GetOrderByIdResponse]
        def getRoot(respond: GetRootResponse.type)(): F[GetRootResponse]
        def putBar(respond: PutBarResponse.type)(bar: Long): F[Response[F]]
      }
    """
    val resource =
      q"""
      class StoreResource[F[_]](mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        implicit val OrderStatusQueryParamDecoder: QueryParamDecoder[OrderStatus] = (value: QueryParameterValue) => Json.fromString(value.value).as[OrderStatus].leftMap(t => ParseFailure("Query decoding failed", t.getMessage)).toValidatedNel
        object GetOrderByIdStatusMatcher extends QueryParamDecoderMatcher[OrderStatus]("status")
        object PutBarBarMatcher extends QueryParamDecoderMatcher[Long]("bar")
        private[this] val getFooBarOkEncoder = jsonEncoderOf[F, Boolean]
        private[this] val getFooBarOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
          def status = org.http4s.Status.Ok
          val liftG = cats.arrow.FunctionK.id
        }
        private[this] val getFooOkEncoder = jsonEncoderOf[F, Boolean]
        private[this] val getFooOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
          def status = org.http4s.Status.Ok
          val liftG = cats.arrow.FunctionK.id
        }
        private[this] val getOrderByIdOkEncoder = jsonEncoderOf[F, Order]
        private[this] val getOrderByIdOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
          def status = org.http4s.Status.Ok
          val liftG = cats.arrow.FunctionK.id
        }
        def routes(handler: StoreHandler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root / "foo" / "" =>
              mapRoute("getFoo", req, {
                handler.getFoo(GetFooResponse)() flatMap ({
                  case resp: GetFooResponse.Ok =>
                    getFooOkEntityResponseGenerator(resp.value)(F, getFooOkEncoder)
                })
              })
            case req @ GET -> Root / "foo" / LongVar(bar) =>
              mapRoute("getFooBar", req, {
                handler.getFooBar(GetFooBarResponse)(bar) flatMap ({
                  case resp: GetFooBarResponse.Ok =>
                    getFooBarOkEntityResponseGenerator(resp.value)(F, getFooBarOkEncoder)
                })
              })
            case req @ GET -> Root / "store" / "order" / LongVar(orderId) :? GetOrderByIdStatusMatcher(status) =>
              mapRoute("getOrderById", req, {
                handler.getOrderById(GetOrderByIdResponse)(orderId, status) flatMap ({
                  case resp: GetOrderByIdResponse.Ok =>
                    getOrderByIdOkEntityResponseGenerator(resp.value)(F, getOrderByIdOkEncoder)
                  case GetOrderByIdResponse.BadRequest =>
                    F.pure(Response[F](status = org.http4s.Status.BadRequest))
                  case GetOrderByIdResponse.NotFound =>
                    F.pure(Response[F](status = org.http4s.Status.NotFound))
                })
              })
            case req @ GET -> Root =>
              mapRoute("getRoot", req, {
                handler.getRoot(GetRootResponse)() flatMap ({
                  case GetRootResponse.Ok =>
                    F.pure(Response[F](status = org.http4s.Status.Ok))
                })
              })
            case req @ PUT -> Root / "bar" :? PutBarBarMatcher(bar) =>
              mapRoute("putBar", req, {
                handler.putBar(PutBarResponse)(bar)
              })
          }
        }
      }
    """

    genHandler.structure shouldEqual handler.structure
    genResource.structure should equal(resource.structure)
  }

  test("Ensure routes are generated with tracing") {
    val (
      _,
      _,
      Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty.copy(tracing = true), Http4s)

    val handler =
      q"""
      trait StoreHandler[F[_]] {
        def getFoo(respond: GetFooResponse.type)()(traceBuilder: TraceBuilder[F]): F[GetFooResponse]
        def getFooBar(respond: GetFooBarResponse.type)(bar: Long)(traceBuilder: TraceBuilder[F]): F[GetFooBarResponse]
        def getOrderById(respond: GetOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed)(traceBuilder: TraceBuilder[F]): F[GetOrderByIdResponse]
        def getRoot(respond: GetRootResponse.type)()(traceBuilder: TraceBuilder[F]): F[GetRootResponse]
        def putBar(respond: PutBarResponse.type)(bar: Long)(traceBuilder: TraceBuilder[F]): F[Response[F]]
      }
    """
    val resource =
      q"""
      class StoreResource[F[_]](trace: String => Request[F] => TraceBuilder[F], mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
        implicit val OrderStatusQueryParamDecoder: QueryParamDecoder[OrderStatus] = (value: QueryParameterValue) => Json.fromString(value.value).as[OrderStatus].leftMap(t => ParseFailure("Query decoding failed", t.getMessage)).toValidatedNel
        object GetOrderByIdStatusMatcher extends QueryParamDecoderMatcher[OrderStatus]("status")
        object PutBarBarMatcher extends QueryParamDecoderMatcher[Long]("bar")
        object usingForGetFoo { def unapply(r: Request[F]): Option[(Request[F], TraceBuilder[F])] = Some(r -> trace("store:getFoo")(r)) }
        object usingForGetFooBar { def unapply(r: Request[F]): Option[(Request[F], TraceBuilder[F])] = Some(r -> trace("completely-custom-label")(r)) }
        object usingForGetOrderById { def unapply(r: Request[F]): Option[(Request[F], TraceBuilder[F])] = Some(r -> trace("store:getOrderById")(r)) }
        object usingForGetRoot { def unapply(r: Request[F]): Option[(Request[F], TraceBuilder[F])] = Some(r -> trace("store:getRoot")(r)) }
        object usingForPutBar { def unapply(r: Request[F]): Option[(Request[F], TraceBuilder[F])] = Some(r -> trace("store:putBar")(r)) }
        private[this] val getFooBarOkEncoder = jsonEncoderOf[F, Boolean]
        private[this] val getFooBarOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
          def status = org.http4s.Status.Ok
          val liftG = cats.arrow.FunctionK.id
        }
        private[this] val getFooOkEncoder = jsonEncoderOf[F, Boolean]
        private[this] val getFooOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
          def status = org.http4s.Status.Ok
          val liftG = cats.arrow.FunctionK.id
        }
        private[this] val getOrderByIdOkEncoder = jsonEncoderOf[F, Order]
        private[this] val getOrderByIdOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
          def status = org.http4s.Status.Ok
          val liftG = cats.arrow.FunctionK.id
        }
        def routes(handler: StoreHandler[F]): HttpRoutes[F] = HttpRoutes.of {
          {
            case req @ GET -> Root / "foo" / "" usingForGetFoo traceBuilder =>
              mapRoute("getFoo", req, {
                handler.getFoo(GetFooResponse)()(traceBuilder) flatMap ({
                  case resp: GetFooResponse.Ok =>
                    getFooOkEntityResponseGenerator(resp.value)(F, getFooOkEncoder)
                })
              })
            case req @ GET -> Root / "foo" / LongVar(bar) usingForGetFooBar traceBuilder =>
              mapRoute("getFooBar", req, {
                handler.getFooBar(GetFooBarResponse)(bar)(traceBuilder) flatMap ({
                  case resp: GetFooBarResponse.Ok =>
                    getFooBarOkEntityResponseGenerator(resp.value)(F, getFooBarOkEncoder)
                })
              })
            case req @ GET -> Root / "store" / "order" / LongVar(orderId) :? GetOrderByIdStatusMatcher(status) usingForGetOrderById traceBuilder =>
              mapRoute("getOrderById", req, {
                handler.getOrderById(GetOrderByIdResponse)(orderId, status)(traceBuilder) flatMap ({
                  case resp: GetOrderByIdResponse.Ok =>
                    getOrderByIdOkEntityResponseGenerator(resp.value)(F, getOrderByIdOkEncoder)
                  case GetOrderByIdResponse.BadRequest =>
                    F.pure(Response[F](status = org.http4s.Status.BadRequest))
                  case GetOrderByIdResponse.NotFound =>
                    F.pure(Response[F](status = org.http4s.Status.NotFound))
                })
              })
            case req @ GET -> Root usingForGetRoot traceBuilder =>
              mapRoute("getRoot", req, {
                handler.getRoot(GetRootResponse)()(traceBuilder) flatMap ({
                  case GetRootResponse.Ok =>
                    F.pure(Response[F](status = org.http4s.Status.Ok))
                })
              })
            case req @ PUT -> Root / "bar" :? PutBarBarMatcher(bar) usingForPutBar traceBuilder =>
              mapRoute("putBar", req, {
                handler.putBar(PutBarResponse)(bar)(traceBuilder)
              })
          }
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
      Servers(Server(_, _, genHandler, genResource :: _) :: Nil, Nil)
    ) = runSwaggerSpec(swagger)(Context.empty.copy(customExtraction = true), Http4s)
    val handler =
      q"""
      trait StoreHandler[F[_], -E] {
        def getFoo(respond: GetFooResponse.type)()(extracted: E): F[GetFooResponse]
        def getFooBar(respond: GetFooBarResponse.type)(bar: Long)(extracted: E): F[GetFooBarResponse]
        def getOrderById(respond: GetOrderByIdResponse.type)(orderId: Long, status: OrderStatus = OrderStatus.Placed)(extracted: E): F[GetOrderByIdResponse]
        def getRoot(respond: GetRootResponse.type)()(extracted: E): F[GetRootResponse]
        def putBar(respond: PutBarResponse.type)(bar: Long)(extracted: E): F[Response[F]]
      }
    """
    val resource =
      q"""
        class StoreResource[F[_], E](customExtract: String => Request[F] => E, mapRoute: (String, Request[F], F[Response[F]]) => F[Response[F]] = (_: String, _: Request[F], r: F[Response[F]]) => r)(implicit F: Async[F]) extends Http4sDsl[F] with CirceInstances {
          implicit val OrderStatusQueryParamDecoder: QueryParamDecoder[OrderStatus] = (value: QueryParameterValue) => Json.fromString(value.value).as[OrderStatus].leftMap(t => ParseFailure("Query decoding failed", t.getMessage)).toValidatedNel
          object GetOrderByIdStatusMatcher extends QueryParamDecoderMatcher[OrderStatus]("status")
          object PutBarBarMatcher extends QueryParamDecoderMatcher[Long]("bar")
          object extractorForGetFoo { def unapply(r: Request[F]): Option[(Request[F], E)] = Some(r -> customExtract("getFoo")(r)) }
          object extractorForGetFooBar { def unapply(r: Request[F]): Option[(Request[F], E)] = Some(r -> customExtract("getFooBar")(r)) }
          object extractorForGetOrderById { def unapply(r: Request[F]): Option[(Request[F], E)] = Some(r -> customExtract("getOrderById")(r)) }
          object extractorForGetRoot { def unapply(r: Request[F]): Option[(Request[F], E)] = Some(r -> customExtract("getRoot")(r)) }
          object extractorForPutBar { def unapply(r: Request[F]): Option[(Request[F], E)] = Some(r -> customExtract("putBar")(r)) }
          private[this] val getFooBarOkEncoder = jsonEncoderOf[F, Boolean]
          private[this] val getFooBarOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
            def status = org.http4s.Status.Ok
            val liftG = cats.arrow.FunctionK.id
          }
          private[this] val getFooOkEncoder = jsonEncoderOf[F, Boolean]
          private[this] val getFooOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
            def status = org.http4s.Status.Ok
            val liftG = cats.arrow.FunctionK.id
          }
          private[this] val getOrderByIdOkEncoder = jsonEncoderOf[F, Order]
          private[this] val getOrderByIdOkEntityResponseGenerator = new org.http4s.dsl.impl.EntityResponseGenerator[F, F] {
            def status = org.http4s.Status.Ok
            val liftG = cats.arrow.FunctionK.id
          }
          def routes(handler: StoreHandler[F, E]): HttpRoutes[F] = HttpRoutes.of {
            {
              case req @ GET -> Root / "foo" / "" extractorForGetFoo extracted =>
                mapRoute("getFoo", req, {
                  handler.getFoo(GetFooResponse)()(extracted) flatMap ({
                    case resp: GetFooResponse.Ok =>
                      getFooOkEntityResponseGenerator(resp.value)(F, getFooOkEncoder)
                  })
                })
              case req @ GET -> Root / "foo" / LongVar(bar) extractorForGetFooBar extracted =>
                mapRoute("getFooBar", req, {
                  handler.getFooBar(GetFooBarResponse)(bar)(extracted) flatMap ({
                    case resp: GetFooBarResponse.Ok =>
                      getFooBarOkEntityResponseGenerator(resp.value)(F, getFooBarOkEncoder)
                  })
                })
              case req @ GET -> Root / "store" / "order" / LongVar(orderId) :? GetOrderByIdStatusMatcher(status) extractorForGetOrderById extracted =>
                mapRoute("getOrderById", req, {
                  handler.getOrderById(GetOrderByIdResponse)(orderId, status)(extracted) flatMap ({
                    case resp: GetOrderByIdResponse.Ok =>
                      getOrderByIdOkEntityResponseGenerator(resp.value)(F, getOrderByIdOkEncoder)
                    case GetOrderByIdResponse.BadRequest =>
                      F.pure(Response[F](status = org.http4s.Status.BadRequest))
                    case GetOrderByIdResponse.NotFound =>
                      F.pure(Response[F](status = org.http4s.Status.NotFound))
                  })
                })
              case req @ GET -> Root extractorForGetRoot extracted =>
                mapRoute("getRoot", req, {
                  handler.getRoot(GetRootResponse)()(extracted) flatMap ({
                    case GetRootResponse.Ok =>
                      F.pure(Response[F](status = org.http4s.Status.Ok))
                  })
                })
              case req @ PUT -> Root / "bar" :? PutBarBarMatcher(bar) extractorForPutBar extracted =>
                mapRoute("putBar", req, {
                  handler.putBar(PutBarResponse)(bar)(extracted)
                })
            }
          }
        }
      """

    genHandler.structure should equal(handler.structure)
    genResource.structure should equal(resource.structure)
  }
}
