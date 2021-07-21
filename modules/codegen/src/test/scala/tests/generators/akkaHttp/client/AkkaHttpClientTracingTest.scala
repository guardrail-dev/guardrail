package tests.generators.akkaHttp.client

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Client, Clients, Context }
import support.SwaggerSpecRunner
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AkkaHttpClientTracingTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  test("Manage child tracing span") {
    val swagger = s"""
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
      |      x-jvm-package: foo.barBaz
      |      parameters:
      |        - name: bleep
      |          in: query
      |          required: true
      |          type: string
      |      responses:
      |        200:
      |          description: Success
      |""".stripMargin

    val (_, Clients(Client(_, _, _, _, cls, _) :: _, Nil), _) =
      runSwaggerSpec(swagger)(Context.empty.copy(tracing = true), AkkaHttp)

    val client = q"""
      class BarBazClient(host: String = "http://localhost:1234", clientName: String = "foo-bar-baz")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
          EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
            entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
          }.recover({
            case t =>
              Left(Left(t))
          }))
        }
        def getFoo(traceBuilder: TraceBuilder, bleep: String, methodName: String = "get-foo", headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetFooResponse] = {
          val tracingHttpClient = traceBuilder(s"$$clientName:$$methodName")(httpClient)
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo" + "?" + Formatter.addArg("bleep", bleep), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(tracingHttpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetFooResponse.OK))
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

  test("Manage child span with tags") {
    val swagger = s"""
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
      |      tags: ["foo", "barBaz"]
      |      x-jvm-package: foo.barBaz
      |      operationId: getFoo
      |      responses:
      |        200:
      |          description: Success
      |""".stripMargin

    val (
      _,
      Clients(Client(tags, className, _, _, cls, _) :: _, Nil),
      _
    ) = runSwaggerSpec(swagger)(Context.empty.copy(tracing = true), AkkaHttp)

    val client = q"""
      class BarBazClient(host: String = "http://localhost:1234", clientName: String = "foo-bar-baz")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
          EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
            entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
          }.recover({
            case t =>
              Left(Left(t))
          }))
        }
        def getFoo(traceBuilder: TraceBuilder, methodName: String = "get-foo", headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetFooResponse] = {
          val tracingHttpClient = traceBuilder(s"$$clientName:$$methodName")(httpClient)
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(tracingHttpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetFooResponse.OK))
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
