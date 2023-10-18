package tests.generators.akkaHttp.client

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.meta._

import support.{ ScalaMetaMatchers, SwaggerSpecRunner }

import dev.guardrail.Context
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.generators.{ Client, Clients }
import dev.guardrail.terms.protocol.{ ClassDefinition, RandomType }

class BasicTest extends AnyFunSuite with Matchers with SwaggerSpecRunner with ScalaMetaMatchers {
  val spec: String = s"""
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
      ProtocolDefinitions(RandomType(_, tpe) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

    tpe should matchStructure(t"io.circe.Json")
  }

  test("Handle json subvalues") {
    val (
      ProtocolDefinitions(_ :: ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Blix(map: io.circe.Json)
    """

    val companion = q"""
      object Blix {
        implicit val encodeBlix: _root_.io.circe.Encoder.AsObject[Blix] = {
          _root_.io.circe.Encoder.AsObject.instance[Blix](a => _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("map", a.map.asJson))))
        }
        implicit val decodeBlix: _root_.io.circe.Decoder[Blix] = new _root_.io.circe.Decoder[Blix] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[Blix] = for (v0 <- c.downField("map").as[io.circe.Json]) yield Blix(v0) }
      }
    """

    cls should matchStructure(definition)
    cmp should matchStructure(companion)
  }

  test("Properly handle all methods") {
    val (
      _,
      Clients(Client(tags, className, _, _, cls, _) :: _, Nil),
      _
    ) = runSwaggerSpec(scalaInterpreter)(spec)(Context.empty, "akka-http")

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
        val getBazOKDecoder = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[io.circe.Json].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        def getBar(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetBarResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/bar", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetBarResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def getBaz(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetBazResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/baz", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[io.circe.Json](getBazOKDecoder, implicitly, implicitly).map(x => Right(GetBazResponse.OK(x)))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def postFoo(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PostFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(PostFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def getFoo(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], GetFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(GetFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def putFoo(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PutFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PUT, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(PutFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def patchFoo(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PatchFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.PATCH, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(PatchFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def deleteFoo(headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], DeleteFooResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.DELETE, host + basePath + "/foo", allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              resp.discardEntityBytes().future.map(_ => Right(DeleteFooResponse.OK))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cls.head.value should matchStructure(client)
  }
}
