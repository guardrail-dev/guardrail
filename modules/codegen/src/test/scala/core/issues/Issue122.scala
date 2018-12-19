package tests.core.issues

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.{
  ClassDefinition,
  Client,
  ClientGenerator,
  Clients,
  CodegenApplication,
  Context,
  ProtocolDefinitions,
  ProtocolGenerator,
  RandomType,
  Target
}
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class Issue122 extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /user/{id}:
    |    get:
    |      operationId: getUser
    |      x-scala-package: users
    |      produces:
    |        - application/json
    |      parameters:
    |      - name: optionalIterable
    |        in: formData
    |        description: Media Urls
    |        required: false
    |        type: array
    |        items:
    |          type: string
    |      - name: requiredIterable
    |        in: formData
    |        description: Media Urls
    |        required: true
    |        type: array
    |        items:
    |          type: string
    |      responses:
    |        200:
    |          description: success
    |""".stripMargin

  test("Ensure clients are able to pass sequences of values for array form parameters") {
    val (
      _,
      Clients(Client(tags, className, imports, cmp, cls, _) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)

    val client = q"""
      class UsersClient(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
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
        def getUser(optionalIterable: Option[Iterable[String]] = None, requiredIterable: Iterable[String], headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/user/", allHeaders, FormData(List(optionalIterable.toList.flatMap {
            x => x.toList.map(("optionalIterable", _))
          }, List(("requiredIterable", Formatter.show(requiredIterable)))).flatten: _*), HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
      }
    """

    val companion = q"""
      object UsersClient {
        def apply(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): UsersClient = new UsersClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
        def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): UsersClient = new UsersClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
      }
    """

    cls.structure shouldBe client.structure
    cmp.structure shouldBe companion.structure
  }
}
