package tests.generators.akkaHttp.client.contentType

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.generators.syntax.Scala.companionForStaticDefns
import dev.guardrail.{ Client, Clients, Context }
import support.SwaggerSpecRunner
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TextPlainTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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
    |    put:
    |      operationId: putFoo
    |      consumes:
    |        - text/plain
    |      parameters:
    |        - name: body
    |          in: body
    |          required: true
    |          schema:
    |            type: string
    |      responses:
    |        200:
    |          description: Success
    |""".stripMargin

  test("Properly handle all methods") {
    val (
      _,
      Clients(Client(tags, className, _, staticDefns, cls, _) :: _, Nil),
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val companion = q"""
      object Client {
        def apply(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): Client = new Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
        def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): Client = new Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
      }
    """
    val client    = q"""
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
        def putFoo(body: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PutFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PUT, host + basePath + "/foo", allHeaders, TextPlain(body), HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(PutFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cmp.structure should equal(companion.structure)
    cls.head.value.structure should equal(client.structure)
  }
}
