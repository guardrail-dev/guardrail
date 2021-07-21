package tests.generators.akkaHttp.client

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Client, Clients, Context }
import support.SwaggerSpecRunner
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HttpBodiesTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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
      Clients(Client(tags, className, _, _, cls, _) :: _, Nil),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

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
        def postFoo(body: Foo, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PostFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(PostFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def getFoo(body: Foo, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def putFoo(body: Foo, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PutFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PUT, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(PutFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def patchFoo(body: Foo, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PatchFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PATCH, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(PatchFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def deleteFoo(body: Foo, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], DeleteFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.DELETE, host + basePath + "/foo", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(DeleteFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cls.head.value.structure should equal(client.structure)
  }
}
