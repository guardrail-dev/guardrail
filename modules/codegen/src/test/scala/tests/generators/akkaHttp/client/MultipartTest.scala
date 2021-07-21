package tests.generators.akkaHttp.client

import dev.guardrail.generators.Scala.AkkaHttp
import dev.guardrail.{ Client, Clients, Context }
import support.SwaggerSpecRunner
import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MultipartTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {
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
    |    post:
    |      operationId: createFoo
    |      consumes:
    |        - multipart/form-data
    |      produces:
    |        - application/json
    |      parameters:
    |        - name: bar
    |          in: formData
    |          type: string
    |          required: true
    |        - name: Quux
    |          in: formData
    |          type: file
    |          required: true
    |        - name: Baz
    |          in: formData
    |          type: string
    |        - name: oort_cloud
    |          in: formData
    |          type: file
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            type: string
    |""".stripMargin

  test("Multipart form data") {
    val (
      _,
      Clients(Client(_, className, _, _, cls, _) :: _, Nil),
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
        val createFooOKDecoder = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[String].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        def createFoo(bar: String, quux: BodyPartEntity, baz: Option[String] = None, oortCloud: Option[BodyPartEntity] = None, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], CreateFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/foo", allHeaders, Multipart.FormData(Source.fromIterator {
            () => List(Some(Multipart.FormData.BodyPart("bar", Formatter.show(bar))), Some(Multipart.FormData.BodyPart("Quux", quux)), baz.map(v => Multipart.FormData.BodyPart("Baz", Formatter.show(v))), oortCloud.map(v => Multipart.FormData.BodyPart("oort_cloud", v))).flatten.iterator
          }), HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[String](createFooOKDecoder, implicitly, implicitly).map(x => Right(CreateFooResponse.OK(x)))
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
