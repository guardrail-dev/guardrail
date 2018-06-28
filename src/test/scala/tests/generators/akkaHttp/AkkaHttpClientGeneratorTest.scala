package tests.generators.akkaHttp

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Client, Clients, Context }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

class AkkaHttpClientGeneratorTest extends FunSuite with Matchers with SwaggerSpecRunner {
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
      Clients(Client(tags, className, statements) :: Nil),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    tags should equal(Seq("store"))

    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

    val companion = q"""
    object StoreClient {
      def apply(host: String = "http://petstore.swagger.io")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): StoreClient =
        new StoreClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
      def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://petstore.swagger.io")(implicit ec: ExecutionContext, mat: Materializer): StoreClient =
        new StoreClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
    }
    """

    val client = q"""
    class StoreClient(host: String = "http://petstore.swagger.io")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
      val basePath: String = ""
      private[this] def wrap[T: FromEntityUnmarshaller](resp: Future[HttpResponse]): EitherT[Future, Either[Throwable, HttpResponse], T] = {
        EitherT(resp.flatMap(resp => if (resp.status.isSuccess) {
          Unmarshal(resp.entity).to[T].map(Right.apply _)
        } else {
          FastFuture.successful(Left(Right(resp)))
        }).recover({
          case e: Throwable =>
            Left(Left(e))
        }))
      }
      def getOrderById(orderId: Long, headerMeThis: String, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], Order] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]](Some(RawHeader("HeaderMeThis", Formatter.show(headerMeThis)))).flatten
        wrap[Order](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          httpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/store/order/" + Formatter.addPath(orderId), entity = entity, headers = allHeaders))
        })
      }
      def deleteOrder(orderId: Long, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[IgnoredEntity](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          httpClient(HttpRequest(method = HttpMethods.DELETE, uri = host + basePath + "/store/order/" + Formatter.addPath(orderId), entity = entity, headers = allHeaders))
        })
      }
    }
    """

    cmp.structure should equal(companion.structure)
    cls.structure should equal(client.structure)
  }

  test("Ensure traced responses are generated") {
    val (
      _,
      Clients(List(Client(tags, className, statements))),
      _
    ) = runSwaggerSpec(swagger)(Context.empty.copy(framework = Some("akka-http"), tracing = true), AkkaHttp, defaults.akkaGeneratorSettings)

    tags should equal(Seq("store"))

    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

    val companion = q"""
    object StoreClient {
      def apply(host: String = "http://petstore.swagger.io", clientName: String = "store")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): StoreClient =
        new StoreClient(host = host, clientName = clientName)(httpClient = httpClient, ec = ec, mat = mat)
      def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://petstore.swagger.io", clientName: String = "store")(implicit ec: ExecutionContext, mat: Materializer): StoreClient =
        new StoreClient(host = host, clientName = clientName)(httpClient = httpClient, ec = ec, mat = mat)
    }
    """

    val client = q"""
    class StoreClient(host: String = "http://petstore.swagger.io", clientName: String = "store")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
      val basePath: String = ""
      private[this] def wrap[T: FromEntityUnmarshaller](resp: Future[HttpResponse]): EitherT[Future, Either[Throwable, HttpResponse], T] = {
        EitherT(resp.flatMap(resp => if (resp.status.isSuccess) {
          Unmarshal(resp.entity).to[T].map(Right.apply _)
        } else {
          FastFuture.successful(Left(Right(resp)))
        }).recover({
          case e: Throwable =>
            Left(Left(e))
        }))
      }
      def getOrderById(traceBuilder: TraceBuilder, orderId: Long, headerMeThis: String, methodName: String = "get-order-by-id", headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], Order] = {
        val tracingHttpClient = traceBuilder(s"$$clientName:$$methodName")(httpClient)
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]](Some(RawHeader("HeaderMeThis", Formatter.show(headerMeThis)))).flatten
        wrap[Order](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          tracingHttpClient(HttpRequest(method = HttpMethods.GET, uri = host + basePath + "/store/order/" + Formatter.addPath(orderId), entity = entity, headers = allHeaders))
        })
      }
      def deleteOrder(traceBuilder: TraceBuilder, orderId: Long, methodName: String = "delete-order", headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
        val tracingHttpClient = traceBuilder(s"$$clientName:$$methodName")(httpClient)
        val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
        wrap[IgnoredEntity](Marshal(HttpEntity.Empty).to[RequestEntity].flatMap { entity =>
          tracingHttpClient(HttpRequest(method = HttpMethods.DELETE, uri = host + basePath + "/store/order/" + Formatter.addPath(orderId), entity = entity, headers = allHeaders))
        })
      }
    }
    """

    cmp.structure should equal(companion.structure)
    cls.structure should equal(client.structure)
  }
}
