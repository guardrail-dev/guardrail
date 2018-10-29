package tests.core

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._

import scala.meta._

class DereferencingAliasesSpec extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |paths:
    |  /foo:
    |    post:
    |      operationId: doFoo
    |      x-scala-package: foo
    |      parameters:
    |      - in: query
    |        name: long
    |        type: integer
    |        format: int64
    |      - in: body
    |        name: body
    |        type: object
    |        schema:
    |          $$ref: '#/definitions/propRef'
    |      responses:
    |        '200':
    |          description: "IndexedSeq[IndexedSeq[Long]]"
    |          type: object
    |          schema:
    |            $$ref: '#/definitions/defArrayArrayLong'
    |definitions:
    |  defLong:
    |    type: integer
    |    format: int64
    |  defArrayLong:
    |    type: array
    |    items:
    |      type: integer
    |      format: int64
    |  defArrayArrayLong:
    |    type: array
    |    items:
    |      $$ref: '#/definitions/defArrayLong'
    |  propRef:
    |    type: object
    |    properties:
    |      param:
    |        $$ref: "#/definitions/defLong"
    |      array:
    |        $$ref: "#/definitions/defArrayLong"
    |      arrayArray:
    |        $$ref: "#/definitions/defArrayArrayLong"
    |""".stripMargin

  test("All types should be dereferenced") {
    val (
      ProtocolDefinitions(_ :: _ :: _ :: ClassDefinition(_, _, cls, cmp, _) :: _, _, _, _),
      Clients(Client(_, _, statements) :: _),
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)
    val List(clientCmp, clientCls) =
      statements.dropWhile(_.isInstanceOf[Import])

    val definition = q"""
      case class propRef(param: Option[Long] = None, array: Option[IndexedSeq[Long]] = None, arrayArray: Option[IndexedSeq[IndexedSeq[Long]]] = None)
    """

    val companion = q"""
      object propRef {
        implicit val encodepropRef = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct3("param", "array", "arrayArray")( (o: propRef) => (o.param, o.array, o.arrayArray) ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodepropRef = Decoder.forProduct3("param", "array", "arrayArray")(propRef.apply _)
      }
    """

    val clientCompanion = q"""
      object FooClient {
        def apply(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer): FooClient =
          new FooClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
        def httpClient(httpClient: HttpRequest => Future[HttpResponse], host: String = "http://localhost:1234")(implicit ec: ExecutionContext, mat: Materializer): FooClient =
          new FooClient(host = host)(httpClient = httpClient, ec = ec, mat = mat)
      }
    """

    val client = q"""
      class FooClient(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
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
        def doFoo(long: Option[Long] = None, body: Option[propRef] = None, headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IndexedSeq[IndexedSeq[Long]]] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/foo" + "?" + Formatter.addArg("long", long), allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IndexedSeq[IndexedSeq[Long]]](httpClient, req))
        }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
    clientCmp.structure should equal(clientCompanion.structure)
    clientCls.structure should equal(client.structure)
  }
}
