package tests.generators.http4s

import com.twilio.guardrail._
import com.twilio.guardrail.generators.Http4s
import org.scalatest.{ FunSuite, Matchers }
import support.SwaggerSpecRunner
import com.twilio.guardrail.tests._
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
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s, defaults.http4sGeneratorSettings)

    tpe.structure should equal(t"io.circe.Json".structure)
  }

  test("Handle json subvalues") {
    val (
      ProtocolDefinitions(_ :: ClassDefinition(_, _, cls, cmp, _) :: _, _, _, _),
      _,
      _
    ) = runSwaggerSpec(swagger)(Context.empty, Http4s, defaults.http4sGeneratorSettings)

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
    )          = runSwaggerSpec(swagger)(Context.empty, Http4s, defaults.http4sGeneratorSettings)
    val actual = statements.dropWhile(_.isInstanceOf[Import])

    val expected = List(
      q"""object Client {
      def apply[F[_]](host: String = "http://localhost:1234")(implicit effect: Effect[F], httpClient: Http4sClient[F]): Client[F] = new Client[F](host = host)(effect = effect, httpClient = httpClient)
      def httpClient[F[_]](httpClient: Http4sClient[F], host: String = "http://localhost:1234")(implicit effect: Effect[F]): Client[F] = new Client[F](host = host)(effect = effect, httpClient = httpClient)
    }""",
      q"""class Client[F[_]](host: String = "http://localhost:1234")(implicit effect: Effect[F], httpClient: Http4sClient[F]) {
      val basePath: String = ""
      val getBazOkDecoder = EntityDecoder[F, String].flatMapR[io.circe.Json] {
        str => Json.fromString(str).as[io.circe.Json].fold(failure => DecodeResult.failure(InvalidMessageBodyFailure(s"Could not decode response: $$str", Some(failure))), DecodeResult.success(_))
      }
      def getFoo(headers: List[Header] = List.empty): F[GetFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(_) =>
            effect.pure(GetFooResponse.Ok)
        })
      }
      def putFoo(headers: List[Header] = List.empty): F[PutFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.PUT, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(_) =>
            effect.pure(PutFooResponse.Ok)
        })
      }
      def postFoo(headers: List[Header] = List.empty): F[PostFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.POST, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(_) =>
            effect.pure(PostFooResponse.Ok)
        })
      }
      def deleteFoo(headers: List[Header] = List.empty): F[DeleteFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.DELETE, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(_) =>
            effect.pure(DeleteFooResponse.Ok)
        })
      }
      def patchFoo(headers: List[Header] = List.empty): F[PatchFooResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.PATCH, uri = Uri.unsafeFromString(host + basePath + "/foo"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(_) =>
            effect.pure(PatchFooResponse.Ok)
        })
      }
      def getBar(headers: List[Header] = List.empty): F[GetBarResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/bar"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(_) =>
            effect.pure(GetBarResponse.Ok)
        })
      }
      def getBaz(headers: List[Header] = List.empty): F[GetBazResponse] = {
        val allHeaders = headers ++ List[Option[Header]]().flatten
        val req = Request[F](method = Method.GET, uri = Uri.unsafeFromString(host + basePath + "/baz"), headers = Headers(allHeaders))
        httpClient.fetch(req)({
          case Ok(resp) =>
            getBazOkDecoder.decode(resp, strict = false).fold(throw _, identity).map(GetBazResponse.Ok)
        })
      }
    }""",
      q"""sealed abstract class GetFooResponse""",
      q"""object GetFooResponse { case object Ok extends GetFooResponse }""",
      q"""sealed abstract class PutFooResponse""",
      q"""object PutFooResponse { case object Ok extends PutFooResponse }""",
      q"""sealed abstract class PostFooResponse""",
      q"""object PostFooResponse { case object Ok extends PostFooResponse }""",
      q"""sealed abstract class DeleteFooResponse""",
      q"""object DeleteFooResponse { case object Ok extends DeleteFooResponse }""",
      q"""sealed abstract class PatchFooResponse""",
      q"""object PatchFooResponse { case object Ok extends PatchFooResponse }""",
      q"""sealed abstract class GetBarResponse""",
      q"""object GetBarResponse { case object Ok extends GetBarResponse }""",
      q"""sealed abstract class GetBazResponse""",
      q"""object GetBazResponse { case class Ok(value: io.circe.Json) extends GetBazResponse }"""
    )

    expected.map(_.structure) should equal(actual.map(_.structure))
  }
}
