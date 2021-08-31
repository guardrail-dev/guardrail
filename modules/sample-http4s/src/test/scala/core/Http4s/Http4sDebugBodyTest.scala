package core.Http4s

import _root_.debugBody.client.{ http4s => generatedClient }
import _root_.debugBody.server.http4s._
import _root_.debugBody.server.http4s.definitions._
import cats.effect.IO
import cats.effect.IO._
import org.http4s.client.Client
import org.http4s.implicits._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.circe._
import org.http4s._
import org.http4s.syntax._
import org.http4s.circe.CirceInstances
import cats.syntax.all._
import cats.effect.syntax.all._
import fs2._

class Http4sDebugBodyTest extends AnyFunSuite with Matchers with EitherValues with CirceInstances {

  private val server = new Resource[IO]()
    .routes(
      new Handler[IO] {
        override def debugBody(respond: DebugBodyResponse.type)(body: Body): IO[DebugBodyResponse] =
          IO.pure(respond.NoContent)
      }
    )
    .orNotFound

  private def sendRequest(server: HttpApp[IO], body: Json) = {
    val req = Request[IO](
      method = Method.POST,
      uri = Uri.unsafeFromString("http://localhost:1234/bar"),
      body = jsonEncoderOf[IO, Json].toEntity(body).body
    )
    Client.fromHttpApp[IO](server).run(req)
  }

  test("should fail with a helpful error message") {
    val invalidJson = Json.obj(
      ("invalidSomething1") -> Json.fromInt(1),
      ("something2")        -> Json.fromString("something")
    )
    val request        = sendRequest(server, invalidJson)
    val actualResponse = request.use(_.pure[IO]).unsafeRunSync()
    val actualErrorMessages =
      Stream
        .resource(request)
        .flatMap(_.bodyText)
        .compile
        .toList
        .unsafeRunSync()

    val expectedErrorMessage =
      "The request body was invalid. Attempt to decode value on failed cursor: DownField(something1)"

    actualErrorMessages should (have length (1))
    actualErrorMessages.head should equal(expectedErrorMessage)
    actualResponse.status should equal(Status.UnprocessableEntity)
  }

  test("should succeed when body is valid") {
    val validJson = Json.obj(
      ("something1") -> Json.fromInt(1),
      ("something2") -> Json.fromString("something")
    )
    val request        = sendRequest(server, validJson)
    val actualResponse = request.use(_.pure[IO]).unsafeRunSync()

    actualResponse.status should equal(Status.NoContent)
  }
}
