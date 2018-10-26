package tests.core.issues

import _root_.io.swagger.parser.SwaggerParser
import cats.instances.all._
import com.twilio.swagger._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail._
import com.twilio.guardrail.EnumDefinition
import com.twilio.guardrail.tests._
import org.scalatest.{FunSuite, Matchers}
import support.SwaggerSpecRunner

import scala.meta._

class Issue33 extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger: String = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |definitions:
    |  Bar:
    |    type: string
    |    enum:
    |      - v1
    |      - v2
    |paths:
    |  /foo:
    |    get:
    |      operationId: getFoo
    |      parameters:
    |      - name: CustomHeader
    |        in: header
    |        type: string
    |        x-scala-type: Bar
    |        required: true
    |      - name: CustomFormData
    |        in: formData
    |        type: string
    |        x-scala-type: Bar
    |      responses:
    |        '200':
    |          description: OK
    |        '400':
    |          description: Not found
    |""".stripMargin

  test("custom header types respect specified type parameters") {
    val (
      ProtocolDefinitions(EnumDefinition(_name, tpe, _elms, _cls, _cmp) :: Nil, _protoImports, _pkgImports, _pkgObjectContents),
      Clients(Client(_tags, _className, statements) :: _),
      _server
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

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
        def getFoo(customFormData: Option[Bar] = None, customHeader: Bar, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]](Some(RawHeader("CustomHeader", Formatter.show(customHeader)))).flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, FormData(List(("CustomFormData", customFormData.map(Formatter.show(_)))).collect({
            case (n, Some(v)) =>
              (n, v)
          }): _*), HttpProtocols.HTTP/1.1).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
      }
    """

    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

    tpe.structure shouldBe t"Bar".structure
    cls should equal(client.structure)
  }

}
