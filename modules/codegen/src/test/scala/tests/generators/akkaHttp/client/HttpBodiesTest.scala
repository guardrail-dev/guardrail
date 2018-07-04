package tests.generators.akkaHttp.client

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Client, Clients, Context }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
import scala.meta._

class HttpBodiesTest extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /foo:
    |    get:
    |      operationId: getFoo
    |      parameters:
    |        - name: body
    |          in: body
    |          required: true
    |          schema:
    |            $$ref: "#/definitions/Foo"
    |      responses:
    |        200:
    |          description: Success
    |    put:
    |      operationId: putFoo
    |      parameters:
    |        - name: body
    |          in: body
    |          required: true
    |          schema:
    |            $$ref: "#/definitions/Foo"
    |      responses:
    |        200:
    |          description: Success
    |    post:
    |      operationId: postFoo
    |      parameters:
    |        - name: body
    |          in: body
    |          required: true
    |          schema:
    |            $$ref: "#/definitions/Foo"
    |      responses:
    |        200:
    |          description: Success
    |    delete:
    |      operationId: deleteFoo
    |      parameters:
    |        - name: body
    |          in: body
    |          required: true
    |          schema:
    |            $$ref: "#/definitions/Foo"
    |      responses:
    |        200:
    |          description: Success
    |    patch:
    |      operationId: patchFoo
    |      parameters:
    |        - name: body
    |          in: body
    |          required: true
    |          schema:
    |            $$ref: "#/definitions/Foo"
    |      responses:
    |        200:
    |          description: Success
    |definitions:
    |  Foo:
    |    type: object
    |    required:
    |      - map
    |    properties:
    |      map:
    |        type: object
    |""".stripMargin

  test("Properly handle all methods") {
    val (
      _,
      Clients(Client(tags, className, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val Seq(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

    val client = q"""
      class Client(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
          EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
            entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
          }.recover({
            case t =>
              Left(Left(t))
          }))
        }
        private[this] def wrap[T: FromEntityUnmarshaller](client: HttpClient, request: HttpRequest): EitherT[Future, Either[Throwable, HttpResponse], T] = {
          EitherT(client(request).flatMap(resp => if (resp.status.isSuccess) {
            Unmarshal(resp.entity).to[T].map(Right.apply _)
          } else {
            FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          }))
        }
        def getFoo(body: Foo, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def putFoo(body: Foo, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PUT, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def postFoo(body: Foo, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def deleteFoo(body: Foo, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.DELETE, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def patchFoo(body: Foo, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PATCH, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
      }
    """

    cls.structure should equal(client.structure)
  }
}
