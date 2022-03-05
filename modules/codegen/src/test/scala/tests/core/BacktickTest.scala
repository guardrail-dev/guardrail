package tests.core

import scala.meta._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import support.SwaggerSpecRunner

import dev.guardrail.Context
import dev.guardrail.generators.scala.syntax.companionForStaticDefns
import dev.guardrail.generators.{ Client, Clients, ProtocolDefinitions }
import dev.guardrail.terms.protocol.{ ClassDefinition, EnumDefinition }
import dev.guardrail.generators.scala.ScalaGeneratorMappings.scalaInterpreter

class BacktickTest extends AnyFunSuite with Matchers with SwaggerSpecRunner {

  val swagger = s"""
    |swagger: "2.0"
    |info:
    |  title: Whatever
    |  version: 1.0.0
    |host: localhost:1234
    |schemes:
    |  - http
    |paths:
    |  /dashy-route:
    |    get:
    |      tags: ["dashy-package"]
    |      x-jvm-package: dashy-package
    |      operationId: dashy-op-id
    |      produces:
    |        - application/json
    |      parameters:
    |        - name: dashy-parameter
    |          in: query
    |          required: true
    |          type: string
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            $$ref: "#/definitions/dashy-class"
    |    post:
    |      tags: ["dashy-package"]
    |      x-jvm-package: dashy-package
    |      operationId: postDashy-op-id
    |      parameters:
    |        - name: dashy-parameter
    |          in: query
    |          required: true
    |          type: string
    |      responses:
    |        200:
    |          description: Success
    |          schema:
    |            $$ref: "#/definitions/dashy-class"
    |definitions:
    |  dashy-class:
    |    type: object
    |    properties:
    |      dashy-param:
    |        type: integer
    |        format: int64
    |  dashy-enum:
    |    type: string
    |    enum:
    |      - dashy-value-a
    |      - dashy-value-b
    |      - dashy-value.c
    |""".stripMargin

  test("Ensure paths are generated with escapes") {
    val (
      _,
      Clients(Client(tags, className, imports, staticDefns, cls, _) :: _, Nil),
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    tags should equal(Seq("dashy-package"))

    val client = q"""
      class DashyPackageClient(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
          EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
            entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
          }.recover({
            case t =>
              Left(Left(t))
          }))
        }
        val postDashyOpIdOKDecoder = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[DashyClass].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        val dashyOpIdOKDecoder = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[DashyClass].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        def postDashyOpId(dashyParameter: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], PostDashyOpIdResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/dashy-route" + "?" + Formatter.addArg("dashy-parameter", dashyParameter), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[DashyClass](postDashyOpIdOKDecoder, implicitly, implicitly).map(x => Right(PostDashyOpIdResponse.OK(x)))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def dashyOpId(dashyParameter: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], DashyOpIdResponse] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/dashy-route" + "?" + Formatter.addArg("dashy-parameter", dashyParameter), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[DashyClass](dashyOpIdOKDecoder, implicitly, implicitly).map(x => Right(DashyOpIdResponse.OK(x)))
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
    cls.head.value.toString should include("class DashyPackageClient")
    cls.head.value.toString should include("def dashyOpId")
    cls.head.value.toString should include("dashyParameter: String")
    cls.head.value.toString should include("\"dashy-parameter\", dashyParameter")
    cls.head.value.toString shouldNot include("``")

    // Note regarding: def ${Term.Name("post /dashy-route")}
    //   This matches the expected behavior of scala.meta regarding terms that contain spaces.
    //   Were this term name to be wrapped in backticks, the test would fail due to scalameta
    //   automatically stripping the backticks. The following test ensures that even though
    //   the test doesn't follow the pattern, the generated code is still escaped.
    //   This behavior may change in scalameta 2.0.0+
    cls.head.value.toString should include("def postDashyOpId(dashyParameter")

    cmp.toString shouldNot include("``")
  }

  test("Ensure dtos are generated with escapes") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
    case class DashyClass(dashyParam: Option[Long] = None)
    """
    val companion = q"""
      object DashyClass {
        implicit val encodeDashyClass: _root_.io.circe.Encoder.AsObject[DashyClass] = {
          val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String]()
          _root_.io.circe.Encoder.AsObject.instance[DashyClass](a =>  _root_.io.circe.JsonObject.fromIterable(_root_.scala.Vector(("dashy-param", a.dashyParam.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeDashyClass: _root_.io.circe.Decoder[DashyClass] = new _root_.io.circe.Decoder[DashyClass] { final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[DashyClass] = for (v0 <- c.downField("dashy-param").as[Option[Long]]) yield DashyClass(v0) }
      }
    """

    cls.structure should equal(definition.structure)
    cls.toString should include("case class DashyClass")
    cls.toString should include("dashyParam")
    cls.toString shouldNot include("``")

    cmp.structure should equal(companion.structure)
    cmp.toString should include("encodeDashyClass")
    cmp.toString should include("decodeDashyClass")
    cmp.toString should include("DashyClass(v0)")
    cmp.toString shouldNot include(".dashy-")
    cmp.toString shouldNot include("[dashy-")
    cmp.toString shouldNot include("= dashy-")
    cmp.toString shouldNot include("``")
  }

  test("Ensure enums are generated with escapes") {
    val (
      ProtocolDefinitions(_ :: EnumDefinition(_, _, _, _, cls, staticDefns) :: _, _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(scalaInterpreter)(swagger)(Context.empty, "akka-http")
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
    sealed abstract class DashyEnum(val value: String) extends _root_.scala.Product with _root_.scala.Serializable {
      override def toString: String = value.toString
    }
    """
    val companion = q"""
    object DashyEnum {
      object members {
        case object DashyValueA extends DashyEnum("dashy-value-a")
        case object DashyValueB extends DashyEnum("dashy-value-b")
        case object DashyValueC extends DashyEnum("dashy-value.c")
      }
      val DashyValueA: DashyEnum = members.DashyValueA
      val DashyValueB: DashyEnum = members.DashyValueB
      val DashyValueC: DashyEnum = members.DashyValueC
      val values = _root_.scala.Vector(DashyValueA, DashyValueB, DashyValueC)
      implicit val encodeDashyEnum: _root_.io.circe.Encoder[DashyEnum] = _root_.io.circe.Encoder[String].contramap(_.value)
      implicit val decodeDashyEnum:_root_.io.circe.Decoder[DashyEnum] =_root_.io.circe.Decoder[String].emap(value => from(value).toRight(s"$$value not a member of DashyEnum"))
      implicit val showDashyEnum: Show[DashyEnum] = Show[String].contramap[DashyEnum](_.value)
      def from(value: String): _root_.scala.Option[DashyEnum] = values.find(_.value == value)
      implicit val order: cats.Order[DashyEnum] = cats.Order.by[DashyEnum, Int](values.indexOf)
    }
    """

    cls.structure should equal(definition.structure)
    cls.toString should include("sealed abstract class DashyEnum")
    cls.toString shouldNot include("``")

    cmp.structure should equal(companion.structure)
    cmp.toString should include("val DashyValueA")
    cmp.toString should include("case object DashyValueB")
    cmp.toString should include("encodeDashyEnum")
    cmp.toString should include("decodeDashyEnum")
    cmp.toString should include("def from(value: String): _root_.scala.Option[DashyEnum]")
    cmp.toString should include("DashyValueC")
    cmp.toString shouldNot include(".dashy-")
    cmp.toString shouldNot include("[dashy-")
    cmp.toString shouldNot include("= dashy-")
    cmp.toString shouldNot include("``")
  }
}
