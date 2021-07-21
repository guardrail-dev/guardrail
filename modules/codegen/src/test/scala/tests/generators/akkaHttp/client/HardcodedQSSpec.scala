package tests.generators.akkaHttp.client

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Client, Clients, Context }
import support.SwaggerSpecRunner
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HardcodedQSSpec extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /hardcodedQs?foo=bar:
    |    get:
    |      operationId: getHardcodedQs
    |      parameters:
    |      - name: bar
    |        type: integer
    |        format: int32
    |        in: query
    |      responses:
    |        200:
    |          description: Success
    |  /specViolation?isThisSupported={value}:
    |    get:
    |      operationId: getHardcodedQs
    |      parameters:
    |      - name: value
    |        type: integer
    |        format: int32
    |        in: path
    |      - name: bar
    |        type: integer
    |        format: int32
    |        in: query
    |      responses:
    |        200:
    |          description: Success
    |""".stripMargin

  test("Test all cases") {
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
        def getHardcodedQs(bar: Option[Int] = None, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetHardcodedQsResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/hardcodedQs?foo=bar" + "&" + Formatter.addArg("bar", bar), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetHardcodedQsResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def getHardcodedQs(value: Int, bar: Option[Int] = None, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetHardcodedQsResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/specViolation?isThisSupported=" + Formatter.addPath(value) + "&" + Formatter.addArg("bar", bar), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetHardcodedQsResponse.OK))
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
