package tests.core

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import org.scalatest.{ FunSuite, Matchers }
import scala.meta._
import support.SwaggerSpecRunner

class DereferencingAliasesSpec extends FunSuite with Matchers with SwaggerSpecRunner {

  val swagger = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |consumes:
    | - application/json
    |produces:
    | - application/json
    |paths:
    |  /foo:
    |    post:
    |      operationId: doFoo
    |      x-jvm-package: foo
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
      ProtocolDefinitions(_ :: _ :: _ :: ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _),
      Clients(Client(_, clientName, _, clientStaticDefns, clientCls, _) :: _, Nil),
      _
    )             = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp       = companionForStaticDefns(staticDefns)
    val clientCmp = companionForStaticDefns(clientStaticDefns)

    val definition = q"""
      case class propRef(param: Option[Long] = None, array: Option[IndexedSeq[Long]] = None, arrayArray: Option[IndexedSeq[IndexedSeq[Long]]] = None)
    """

    val companion = q"""
      object propRef {
        implicit val encodepropRef: ObjectEncoder[propRef] = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct3("param", "array", "arrayArray")( (o: propRef) => (o.param, o.array, o.arrayArray) ).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodepropRef: Decoder[propRef] = Decoder.forProduct3("param", "array", "arrayArray")(propRef.apply _)
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
        val doFooOKDecoder = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[IndexedSeq[IndexedSeq[Long]]].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        def doFoo(long: Option[Long] = None, body: Option[propRef] = None, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], DoFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/foo" + "?" + Formatter.addArg("long", long), allHeaders, body, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[IndexedSeq[IndexedSeq[Long]]](doFooOKDecoder, implicitly, implicitly).map(x => Right(DoFooResponse.OK(x)))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
    clientCmp.structure should equal(clientCompanion.structure)
    clientCls.head.right.get.structure should equal(client.structure)
  }
}
