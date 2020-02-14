package tests.generators.http4s

import com.twilio.guardrail._
import com.twilio.guardrail.generators.Http4s
import com.twilio.guardrail.generators.syntax.Scala.companionForStaticDefns
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import scala.meta._

class BasicTest extends FunSuite with Matchers with SwaggerSpecRunner {
  val swagger = s"""
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
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s)

    tpe.structure should equal(t"io.circe.Json".structure)
  }

  test("Handle json subvalues") {
    val (
      ProtocolDefinitions(_ :: ClassDefinition(_, _, _, cls, staticDefns, _) :: _, _, _, _),
      _,
      _
    )       = runSwaggerSpec(swagger)(Context.empty, Http4s)
    val cmp = companionForStaticDefns(staticDefns)

    val definition = q"""
      case class Blix(map: io.circe.Json)
    """

    val companion = q"""
      object Blix {
        implicit val encodeBlix: Encoder.AsObject[Blix] = {
          val readOnlyKeys = Set[String]()
          Encoder.AsObject.instance[Blix](a => JsonObject.fromIterable(Vector(("map", a.map.asJson)))).mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))
        }
        implicit val decodeBlix: Decoder[Blix] = new Decoder[Blix] { final def apply(c: HCursor): Decoder.Result[Blix] = for (v0 <- c.downField("map").as[io.circe.Json]) yield Blix(v0) }
      }
    """

    cls.structure should equal(definition.structure)
    cmp.structure should equal(companion.structure)
  }

  test("Properly handle all methods") {
    val (
      _,
      Clients(Client(tags, className, _, staticDefns, cls, statements) :: _, Nil),
      _
    )       = runSwaggerSpec(swagger)(Context.empty, Http4s)
    val cmp = companionForStaticDefns(staticDefns)

    val companion = q"""object Client {
      def apply[F[_]](host: String = "http://localhost:1234")(implicit F: Async[F], httpClient: Http4sClient[F]): Client[F] = new Client[F](host = host)(F = F, httpClient = httpClient)
      def httpClient[F[_]](httpClient: Http4sClient[F], host: String = "http://localhost:1234")(implicit F: Async[F]): Client[F] = new Client[F](host = host)(F = F, httpClient = httpClient)
    }"""
    val client    = q"""class Client[F[_]](host: String = "http://localhost:1234")(implicit F: Async[F], httpClient: Http4sClient[F]) {
      val basePath: String = ""

      private def parseOptionalHeader(response: Response[F], header: String): F[Option[String]] =
        F.pure(response.headers.get(header.ci).map(_.value))

      private def parseRequiredHeader(response: Response[F], header: String): F[String] =
        response.headers
          .get(header.ci)
          .map(_.value).fold[F[String]](F.raiseError(ParseFailure("Missing required header.", s"HTTP header '$$header' is not present.")))(F.pure)

      private[this] val getBazOkDecoder = jsonOf[F, io.circe.Json]
      def getBar(headers: List[Header] = List.empty): F[GetBarResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/bar"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case _root_.org.http4s.Status.Ok(_) =>
            F.pure(GetBarResponse.Ok): F[GetBarResponse]
          case resp =>
            F.raiseError[GetBarResponse](UnexpectedStatus(resp.status))
        })
      }
      def getBaz(headers: List[Header] = List.empty): F[GetBazResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/baz"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case _root_.org.http4s.Status.Ok(resp) =>
            F.map(getBazOkDecoder.decode(resp, strict = false).value.flatMap(F.fromEither))(GetBazResponse.Ok.apply): F[GetBazResponse]
          case resp =>
            F.raiseError[GetBazResponse](UnexpectedStatus(resp.status))
        })
      }
      def postFoo(headers: List[Header] = List.empty): F[PostFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.POST, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case _root_.org.http4s.Status.Ok(_) =>
            F.pure(PostFooResponse.Ok): F[PostFooResponse]
          case resp =>
            F.raiseError[PostFooResponse](UnexpectedStatus(resp.status))
        })
      }
      def getFoo(headers: List[Header] = List.empty): F[GetFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case _root_.org.http4s.Status.Ok(_) =>
            F.pure(GetFooResponse.Ok): F[GetFooResponse]
          case resp =>
            F.raiseError[GetFooResponse](UnexpectedStatus(resp.status))
        })
      }
      def putFoo(headers: List[Header] = List.empty): F[PutFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.PUT, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case _root_.org.http4s.Status.Ok(_) =>
            F.pure(PutFooResponse.Ok): F[PutFooResponse]
          case resp =>
            F.raiseError[PutFooResponse](UnexpectedStatus(resp.status))
        })
      }
      def patchFoo(headers: List[Header] = List.empty): F[PatchFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.PATCH, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case _root_.org.http4s.Status.Ok(_) =>
            F.pure(PatchFooResponse.Ok): F[PatchFooResponse]
          case resp =>
            F.raiseError[PatchFooResponse](UnexpectedStatus(resp.status))
        })
      }
      def deleteFoo(headers: List[Header] = List.empty): F[DeleteFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.DELETE, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case _root_.org.http4s.Status.Ok(_) =>
            F.pure(DeleteFooResponse.Ok): F[DeleteFooResponse]
          case resp =>
            F.raiseError[DeleteFooResponse](UnexpectedStatus(resp.status))
        })
      }
    }"""
    val expected = List(
      q"""
        sealed abstract class GetBarResponse {
          def fold[A](handleOk: => A): A = this match {
            case GetBarResponse.Ok => handleOk
          }
        }
      """,
      q"""object GetBarResponse { case object Ok extends GetBarResponse }""",
      q"""
        sealed abstract class GetBazResponse {
          def fold[A](handleOk: io.circe.Json => A): A = this match {
            case x: GetBazResponse.Ok =>
              handleOk(x.value)
          }
        }
      """,
      q"""object GetBazResponse { case class Ok(value: io.circe.Json) extends GetBazResponse }""",
      q"""
        sealed abstract class PostFooResponse {
          def fold[A](handleOk: => A): A = this match {
            case PostFooResponse.Ok => handleOk
          }
        }
      """,
      q"""object PostFooResponse { case object Ok extends PostFooResponse }""",
      q"""
        sealed abstract class GetFooResponse {
          def fold[A](handleOk: => A): A = this match {
            case GetFooResponse.Ok => handleOk
          }
        }
      """,
      q"""object GetFooResponse { case object Ok extends GetFooResponse }""",
      q"""
        sealed abstract class PutFooResponse {
          def fold[A](handleOk: => A): A = this match {
            case PutFooResponse.Ok => handleOk
          }
        }
      """,
      q"""object PutFooResponse { case object Ok extends PutFooResponse }""",
      q"""
        sealed abstract class PatchFooResponse {
          def fold[A](handleOk: => A): A = this match {
            case PatchFooResponse.Ok => handleOk
          }
        }
      """,
      q"""object PatchFooResponse { case object Ok extends PatchFooResponse }""",
      q"""
        sealed abstract class DeleteFooResponse {
          def fold[A](handleOk: => A): A = this match {
            case DeleteFooResponse.Ok => handleOk
          }
        }
      """,
      q"""object DeleteFooResponse { case object Ok extends DeleteFooResponse }"""
    )

    expected.zip(statements).foreach({ case (a, b) => a.structure should equal(b.structure) })
    cmp.structure should equal(companion.structure)
    cls.head.right.get.structure should equal(client.structure)
  }
}
