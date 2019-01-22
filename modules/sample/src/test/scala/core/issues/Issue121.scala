package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshaller
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{ EitherValues, FunSuite, Matchers }
import scala.concurrent.Future
import io.circe._
import _root_.jawn.IncompleteParseException

class Issue121Suite extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  test("akka-http server can respond with 204") {
    import issues.issue121.server.akkaHttp.{ Handler, Resource }
    import issues.issue121.server.akkaHttp.definitions._
    val route = Resource.routes(new Handler {
      override def deleteFoo(respond: Resource.deleteFooResponse.type)(id: Long): Future[Resource.deleteFooResponse] =
        Future.successful(respond.NoContent)
    })

    Delete("/entity").withEntity(FormData("id" -> "1234").toEntity) ~> route ~> check {
      status should equal(StatusCodes.NoContent)
      response.entity.contentType should equal(ContentTypes.NoContentType)
      response.entity.contentLengthOption should equal(Some(0))
    }
  }

  test("akka-http client can respond with 204") {
    import issues.issue121.client.akkaHttp.Client
    import issues.issue121.client.akkaHttp.definitions._

    def noContentResponse: HttpRequest => Future[HttpResponse] = _ => Future.successful(HttpResponse(204))

    /* Correct mime type
     * Missing content
     */
    Client
      .httpClient(noContentResponse, "http://localhost:80")
      .deleteFoo(1234)
      .fold(
        _ => failTest("Error"),
        _.fold(
          handleNoContent = ()
        )
      )
      .futureValue
  }

  test("http4s server can respond with 204") {
    import cats.effect.IO
    import issues.issue121.server.http4s.definitions._
    import issues.issue121.server.http4s.{ DeleteFooResponse, Handler, Resource }
    import org.http4s._
    import org.http4s.client.Client
    import org.http4s.client.UnexpectedStatus
    import org.http4s.client.blaze._
    import org.http4s.headers._
    import org.http4s.implicits._
    import org.http4s.multipart._

    val route = new Resource[IO]().routes(new Handler[IO] {
      override def deleteFoo(respond: DeleteFooResponse.type)(id: Long): IO[DeleteFooResponse] =
        IO.pure(respond.NoContent)
    })

    val client = Client.fromHttpApp[IO](route.orNotFound)

    val req = Request[IO](method = Method.DELETE, uri = Uri.unsafeFromString("/entity")).withEntity(UrlForm("id" -> "1234"))

    client
      .fetch(req)({
        case Status.NoContent(resp) =>
          IO.pure({
            resp.status should equal(Status.NoContent)
            resp.contentType should equal(None)
            resp.contentLength should equal(None)
            ()
          })
      })
      .unsafeRunSync()
  }

  test("http4s client can respond with 204") {
    import issues.issue121.client.http4s.Client
    import issues.issue121.client.http4s.definitions._
    import cats.effect.IO
    import cats.data.Kleisli
    import org.http4s._
    import org.http4s.client.{ Client => Http4sClient }
    import org.http4s.client.UnexpectedStatus
    import org.http4s.client.blaze._
    import org.http4s.headers._
    import org.http4s.implicits._
    import org.http4s.multipart._

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
        _ => failTest("Error"),
        _.fold(
          handleNoContent = ()
        )
      )
  }
}
