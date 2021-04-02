package core.issues

import cats.data.NonEmptyList
import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.generators.syntax.Scala.companionForStaticDefns
import dev.guardrail.{ Client, Clients, Context }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import support.SwaggerSpecRunner

import scala.meta._

class Issue313 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |openapi: 3.0.2
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |servers:
    |  - url: http://localhost:1234
    |paths:
    |  /users/{name}:
    |    patch:
    |      operationId: changeUser
    |      parameters:
    |      - name: name
    |        in: path
    |        required: true
    |        schema:
    |          type: string
    |      requestBody:
    |        required: true
    |        content:
    |          multipart/form-data:
    |            schema:
    |              type: object
    |              required:
    |                - id
    |              properties:
    |                id:
    |                  type: integer
    |                  format: int64
    |      responses:
    |        '204': {}
    |""".stripMargin

  test("Test in body generation") {
    val (
      _,
      Clients(Client(tags, className, imports, staticDefns, NonEmptyList(Right(cls), _), _) :: _, Nil),
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

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
        def changeUser(name: String, id: Long, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], ChangeUserResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PATCH, host + basePath + "/users/" + Formatter.addPath(name), allHeaders, Multipart.FormData(Source.fromIterator {
            () => List(Some(Multipart.FormData.BodyPart("id", Formatter.show(id)))).flatten.iterator
          }), HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.NoContent =>
              resp.discardEntityBytes().future.map(_ => Right(ChangeUserResponse.NoContent))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    val companion = q"""
      object Client {
        def apply(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): Client = new Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
        def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): Client = new Client(host = host)(httpClient = httpClient, ec = ec, mat = mat)
      }
    """

    cls.structure shouldBe client.structure
    cmp.structure shouldBe companion.structure
  }
}
