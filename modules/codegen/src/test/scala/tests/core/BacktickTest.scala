package tests.core

import com.twilio.guardrail._
import com.twilio.guardrail.generators.AkkaHttp
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner

import scala.meta._

class BacktickTest extends FunSuite with Matchers with SwaggerSpecRunner {

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
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    tags should equal(Seq("dashy-package"))

    val client = q"""
      class `Dashy-packageClient`(host: String = "http://localhost:1234")(implicit httpClient: HttpRequest => Future[HttpResponse], ec: ExecutionContext, mat: Materializer) {
        val basePath: String = ""
        private[this] def makeRequest[T: ToEntityMarshaller](method: HttpMethod, uri: Uri, headers: scala.collection.immutable.Seq[HttpHeader], entity: T, protocol: HttpProtocol): EitherT[Future, Either[Throwable, HttpResponse], HttpRequest] = {
          EitherT(Marshal(entity).to[RequestEntity].map[Either[Either[Throwable, HttpResponse], HttpRequest]] {
            entity => Right(HttpRequest(method = method, uri = uri, headers = headers, entity = entity, protocol = protocol))
          }.recover({
            case t =>
              Left(Left(t))
          }))
        }
        val `postDashy-op-idOKDecoder` = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[`dashy-class`].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        val `dashy-op-idOKDecoder` = {
          structuredJsonEntityUnmarshaller.flatMap(_ => _ => json => io.circe.Decoder[`dashy-class`].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
        }
        def `postDashy-op-id`(dashyParameter: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], `PostDashy-op-idResponse`] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.POST, host + basePath + "/dashy-route" + "?" + Formatter.addArg("dashy-parameter", dashyParameter), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[`dashy-class`](`postDashy-op-idOKDecoder`, implicitly, implicitly).map(x => Right(`PostDashy-op-idResponse`.OK(x)))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
        def `dashy-op-id`(dashyParameter: String, headers: List[HttpHeader] = Nil): EitherT[Future, Either[Throwable, HttpResponse], `Dashy-op-idResponse`] = {
          val allHeaders = headers ++ scala.collection.immutable.Seq[Option[HttpHeader]]().flatten
          makeRequest(HttpMethods.GET, host + basePath + "/dashy-route" + "?" + Formatter.addArg("dashy-parameter", dashyParameter), allHeaders, HttpEntity.Empty, HttpProtocols.`HTTP/1.1`).flatMap(req => EitherT(httpClient(req).flatMap(resp => resp.status match {
            case StatusCodes.OK =>
              Unmarshal(resp.entity).to[`dashy-class`](`dashy-op-idOKDecoder`, implicitly, implicitly).map(x => Right(`Dashy-op-idResponse`.OK(x)))
            case _ =>
              FastFuture.successful(Left(Right(resp)))
          }).recover({
            case e: Throwable =>
              Left(Left(e))
          })))
        }
      }
    """

    cls.head.right.get.structure should equal(client.structure)
    cls.head.right.get.toString should include("class `Dashy-packageClient`")
    cls.head.right.get.toString should include("def `dashy-op-id`")
    cls.head.right.get.toString should include("dashyParameter: String")
    cls.head.right.get.toString should include("\"dashy-parameter\", dashyParameter")
    cls.head.right.get.toString shouldNot include("``")

    // Note regarding: def ${Term.Name("post /dashy-route")}
    //   This matches the expected behavior of scala.meta regarding terms that contain spaces.
    //   Were this term name to be wrapped in backticks, the test would fail due to scalameta
    //   automatically stripping the backticks. The following test ensures that even though
    //   the test doesn't follow the pattern, the generated code is still escaped.
    //   This behavior may change in scalameta 2.0.0+
    cls.head.right.get.toString should include("def `postDashy-op-id`(dashyParameter")

    cmp.toString shouldNot include("``")
  }

  test("Ensure dtos are generated with escapes") {
    val (
      ProtocolDefinitions(ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
    case class `dashy-class`(dashyParam: Option[Long] = None)
    """
    val companion  = q"""
      object `dashy-class` {
        implicit val `encodedashy-class`: Encoder.AsObject[`dashy-class`] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[`dashy-class`](a => JsonObject.fromIterable(Vector(("dashy-param", a.dashyParam.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val `decodedashy-class`: Decoder[`dashy-class`] = new Decoder[`dashy-class`] { final def apply(c: HCursor): Decoder.Result[`dashy-class`] = for (v0 <- c.downField("dashy-param").as[Option[Long]]) yield `dashy-class`(v0) }
      }
    """

    cls.structure should equal(definition.structure)
    cls.toString should include("case class `dashy-class`")
    cls.toString should include("dashyParam")
    cls.toString shouldNot include("``")

    cmp.structure should equal(companion.structure)
    cmp.toString should include("`encodedashy-class`")
    cmp.toString should include("`decodedashy-class`")
    cmp.toString should include("`dashy-class`(v0)")
    cmp.toString shouldNot include(".dashy-")
    cmp.toString shouldNot include("[dashy-")
    cmp.toString shouldNot include("= dashy-")
    cmp.toString shouldNot include("``")
  }

  test("Ensure enums are generated with escapes") {
    val (
      ProtocolDefinitions(_ :: EnumDefinition(_, _, _, _, cls, staticDefns) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, AkkaHttp)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
    sealed abstract class `dashy-enum`(val value: String) {
      override def toString: String = value.toString
    }
    """
    val companion  = q"""
    object `dashy-enum` {
      object members {
        case object DashyValueA extends `dashy-enum`("dashy-value-a")
        case object DashyValueB extends `dashy-enum`("dashy-value-b")
        case object DashyValueC extends `dashy-enum`("dashy-value.c")
      }
      val DashyValueA: `dashy-enum` = members.DashyValueA
      val DashyValueB: `dashy-enum` = members.DashyValueB
      val DashyValueC: `dashy-enum` = members.DashyValueC
      val values = Vector(DashyValueA, DashyValueB, DashyValueC)
      implicit val `encodedashy-enum`: Encoder[`dashy-enum`] = Encoder[String].contramap(_.value)
      implicit val `decodedashy-enum`: Decoder[`dashy-enum`] = Decoder[String].emap(value => parse(value).toRight(s"$$value not a member of dashy-enum"))
      implicit val `addPathdashy-enum`: AddPath[`dashy-enum`] = AddPath.build(_.value)
      implicit val `showdashy-enum`: Show[`dashy-enum`] = Show.build(_.value)
      def parse(value: String): Option[`dashy-enum`] = values.find(_.value == value)
      implicit val order: cats.Order[`dashy-enum`] = cats.Order.by[`dashy-enum`, Int](values.indexOf)
    }
    """

    cls.structure should equal(definition.structure)
    cls.toString should include("sealed abstract class `dashy-enum`")
    cls.toString shouldNot include("``")

    cmp.structure should equal(companion.structure)
    cmp.toString should include("val DashyValueA")
    cmp.toString should include("case object DashyValueB")
    cmp.toString should include("`encodedashy-enum`")
    cmp.toString should include("`decodedashy-enum`")
    cmp.toString should include("def parse(value: String): Option[`dashy-enum`]")
    cmp.toString should include("DashyValueC")
    cmp.toString shouldNot include(".dashy-")
    cmp.toString shouldNot include("[dashy-")
    cmp.toString shouldNot include("= dashy-")
    cmp.toString shouldNot include("``")
  }
}
