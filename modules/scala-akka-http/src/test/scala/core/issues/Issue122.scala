package tests.core.issues

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._
import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.generators.{ Client, Clients }

class Issue122 extends AnyFunSuite with Matchers with SwaggerSpecRunner {
  val spec: String = s"""
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
    |      x-jvm-package: users
    |      consumes:
    |        - application/x-www-form-urlencoded
    |      produces:
    |        - application/json
    |      parameters:
    |      - name: id
    |        in: path
    |        type: string
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
      Clients(Client(tags, className, imports, staticDefns, cls, _) :: _, Nil),
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

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
        def getUser(id: String, optionalIterable: Option[Vector[String]] = None, requiredIterable: Vector[String], headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetUserResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/user/" + Formatter.addPath(id), allHeaders, FormData(List(optionalIterable.toList.flatMap {
            x => x.toList.map(x => ("optionalIterable", Formatter.show(x)))
          }, requiredIterable.toList.map(x => ("requiredIterable", Formatter.show(x)))).flatten: _*), HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetUserResponse.OK))
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
      object UsersClient {
        def apply(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): UsersClient = new UsersClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
        def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): UsersClient = new UsersClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
      }
    """

    cls.head.value.structure shouldBe client.structure
    cmp.structure shouldBe companion.structure
  }
}
