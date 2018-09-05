package tests.generators.akkaHttp.client

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{ Client, Clients, Context }
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
import scala.meta._

class AkkaHttpClientTracingTest extends FunSuite with Matchers with SwaggerSpecRunner {

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
      |      x-scala-package: foo.barBaz
      |      parameters:
      |        - name: bleep
      |          in: query
      |          required: true
      |          type: string
      |      responses:
      |        200:
      |          description: Success
      |""".stripMargin

    val (_, Clients(Client(_, _, statements) :: _), _) =
      runSwaggerSpec(swagger)(Context.empty.copy(tracing = true), AkkaHttp, defaults.akkaGeneratorSettings)

    val List(_, cls) = statements.dropWhile(_.isInstanceOf[Import])

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
        def getFoo(traceBuilder: TraceBuilder, bleep: String, methodName: String = "get-foo", headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val tracingHttpClient = traceBuilder(s"$$clientName:$$methodName")(httpClient)
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo" + "?" + Formatter.addArg("bleep", bleep), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](tracingHttpClient, req))
        }
      }
    """

    cls.structure should equal(client.structure)
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
      |      x-scala-package: foo.barBaz
      |      operationId: getFoo
      |      responses:
      |        200:
      |          description: Success
      |""".stripMargin

    val (
      _,
      Clients(Client(tags, className, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty.copy(tracing = true), AkkaHttp, defaults.akkaGeneratorSettings)

    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

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
        def getFoo(traceBuilder: TraceBuilder, methodName: String = "get-foo", headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val tracingHttpClient = traceBuilder(s"$$clientName:$$methodName")(httpClient)
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](tracingHttpClient, req))
        }
      }
    """

    cls.structure should equal(client.structure)
  }
}
