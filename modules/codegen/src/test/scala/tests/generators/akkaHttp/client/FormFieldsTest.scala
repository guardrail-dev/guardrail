package tests.generators.akkaHttp.client

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Client, Clients, Context }
import support.SwaggerSpecRunner
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FormFieldsTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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
    |        - multipart/form-data
    |      parameters:
    |        - name: foo
    |          in: formData
    |          type: string
    |          required: true
    |        - name: bar
    |          in: formData
    |          type: integer
    |          format: int64
    |          required: true
    |        - name: baz
    |          in: formData
    |          type: file
    |          required: true
    |      responses:
    |        200:
    |          description: Success
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
        def putFoo(foo: String, bar: Long, baz: BodyPartEntity, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PutFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PUT, host + basePath + "/foo", allHeaders, Multipart.FormData(Source.fromIterator {
            () => List(Some(Multipart.FormData.BodyPart("foo", Formatter.show(foo))), Some(Multipart.FormData.BodyPart("bar", Formatter.show(bar))), Some(Multipart.FormData.BodyPart("baz", baz))).flatten.iterator
          }), HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
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

    cls.head.value.structure should equal(client.structure)
  }
}
