package core.issues

import cats.effect.IO
import cats.data.Kleisli
import org.http4s._
import org.http4s.client.{ Client => Http4sClient }
import org.http4s.blaze.client._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.multipart._
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import io.circe._

class Issue121Suite extends AnyFunSuite with Matchers with EitherValues with ScalaFutures {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  test("http4s server can respond with 204") {
    import issues.issue121.server.http4sV022.{ Handler, Resource }
    import issues.issue121.server.http4sV022.Resource.DeleteFooResponse

    val route = new Resource[IO]().routes(new Handler[IO] {
      override def deleteFoo(respond: DeleteFooResponse.type)(id: Long): IO[DeleteFooResponse] =
        IO.pure(respond.NoContent)
    })

    val client = Http4sClient.fromHttpApp[IO](route.orNotFound)

    val req = Request[IO](method = Method.DELETE, uri = Uri.unsafeFromString("/entity")).withEntity(UrlForm("id" -> "1234"))

    client
      .run(req)
      .use { case Status.NoContent(resp) =>
        IO.pure {
          resp.status should equal(Status.NoContent)
          resp.contentType should equal(None)
          resp.contentLength should equal(None)
          ()
        }
      }
      .unsafeRunSync()
  }

  test("http4s client can respond with 204") {
    import issues.issue121.client.http4sV022.Client

    def noContentResponse: Http4sClient[IO] =
      Http4sClient.fromHttpApp[IO](Kleisli.pure(Response[IO](Status.NoContent)))

    /* Correct mime type
     * Missing content
     */
    Client
      .httpClient(noContentResponse, "http://localhost:80")
      .deleteFoo(1234)
      .attempt
      .unsafeRunSync()
      .fold(
        _ => fail("Error"),
        _.fold(
          handleNoContent = ()
        )
      )
  }
}
