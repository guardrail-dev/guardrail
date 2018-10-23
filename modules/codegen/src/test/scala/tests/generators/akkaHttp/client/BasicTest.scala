package tests.generators.akkaHttp.client

import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail._
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
import scala.meta._

class BasicTest extends FunSuite with Matchers with SwaggerSpecRunner {
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
    |    get:
    |      operationId: getFoo
    |      responses:
    |        200:
    |          description: Success
    |    put:
    |      operationId: putFoo
    |      responses:
    |        200:
    |          description: Success
    |    post:
    |      operationId: postFoo
    |      responses:
    |        200:
    |          description: Success
    |    delete:
    |      operationId: deleteFoo
    |      responses:
    |        200:
    |          description: Success
    |    patch:
    |      operationId: patchFoo
    |      responses:
    |        200:
    |          description: Success
    |  /bar:
    |    get:
    |      operationId: getBar
    |      responses:
    |        200:
    |          type: object
    |  /baz:
    |    get:
    |      operationId: getBaz
    |      responses:
    |        200:
    |          schema:
    |            $$ref: "#/definitions/Baz"
    |definitions:
    |  Baz:
    |    type: object
    |  Blix:
    |    type: object
    |    required:
    |      - map
    |    properties:
    |      map:
    |        type: object
    |""".stripMargin

  test("Generate JSON alias definitions") {
    val (
      ProtocolDefinitions(RandomType(_, tpe) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    tpe.structure should equal(t"io.circe.Json".structure)
  }

  test("Handle json subvalues") {
    val (
      ProtocolDefinitions(_ :: ClassDefinition(_, _, cls, cmp, _) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)

    val definition = q"""
      case class Blix(map: io.circe.Json)
    """

    val companion = q"""
      object Blix {
        implicit val encodeBlix = {
          val readOnlyKeys = Set[String]()
          Encoder.forProduct1("map")((o: Blix) => o.map).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeBlix = Decoder.forProduct1("map")(Blix.apply _)
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Properly handle all methods") {
    val (
      _,
      Clients(Client(tags, className, statements) :: _),
      _
    )                  = runSwaggerSpec(swagger)(Context.empty, AkkaHttp, defaults.akkaGeneratorSettings)
    val List(cmp, cls) = statements.dropWhile(_.isInstanceOf[Import])

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
        def getFoo(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def putFoo(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PUT, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def postFoo(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def deleteFoo(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.DELETE, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def patchFoo(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PATCH, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def getBar(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], IgnoredEntity] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/bar", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[IgnoredEntity](httpClient, req))
        }
        def getBaz(headers: scala.collection.immutable.Seq[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], io.circe.Json] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/baz", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => wrap[io.circe.Json](httpClient, req))
        }
      }
    """

    cls.structure should equal(client.structure)
  }
}
