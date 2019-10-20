package core.issues

import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{Client, Clients, Context}
import org.scalatest.{FunSuite, Matchers}
import support.SwaggerSpecRunner

import scala.meta._

class Issue313 extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger: String = s"""
    |openapi: 3.0.1
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |paths:
    |  /pets:
    |    post:
    |      operationId: addPets
    |      requestBody:
    |        required: true
    |        content:
    |          application/json:
    |            schema:
    |              type: object
    |              properties:
    |                id:
    |                  type: integer
    |                name:
    |                  type: string
    |      responses:
    |        201:
    |          description: Created
    |          content: {}
    |""".stripMargin

  test("Don't generate params for non-form request bodies") {

    val (
      _,
      Clients(Client(_, _, _, staticDefns, cls, _) :: _, Nil),
      _
      )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val client = q"""
      class Client(host: String)(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
          EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
            entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
          }.recover({
            case t =>
              Left(Left(t))
          }))
        }
        def addPets(body: io.circe.Json, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], AddPetsResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/pets", allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.Created =>
              resp.discardEntityBytes().future.map(_ => Right(AddPetsResponse.Created))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cls.head.right.get.structure shouldBe client.structure
  }
}
