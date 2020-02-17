package core.issues

import cats.effect.IO
import cats.data.Kleisli
import org.http4s._
import org.http4s.circe._
import org.http4s.client.{ Client => Http4sClient }
import org.http4s.headers._
import org.http4s.implicits._
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{ EitherValues, FunSuite, Matchers, OptionValues }
import tests.scalatest.EitherTValues

class Issue542Suite extends FunSuite with Matchers with EitherValues with ScalaFutures with EitherTValues with OptionValues {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  test("base64 bytes can be sent") {
    import base64.server.http4s.{ FooResponse, Handler, Resource }
    import base64.server.http4s.definitions.Foo
    import base64.server.http4s.Implicits.Base64String

    val route = new Resource[IO]().routes(new Handler[IO] {
      def foo(respond: FooResponse.type)(): IO[FooResponse] = IO.pure(respond.Ok(Foo(Some(new Base64String("foo".getBytes())))))
    })

    val client = Http4sClient.fromHttpApp[IO](route.orNotFound)

    val req = Request[IO](method = Method.GET, uri = Uri.unsafeFromString("/foo"))

    client
      .fetch(req)({
        case Status.Ok(resp) =>
          resp.status should equal(Status.Ok)
          resp.contentType should equal(Some(`Content-Type`(MediaType.application.json)))
          resp.contentLength should equal(Some(16))
          jsonOf[IO, Foo].decode(resp, strict = false).rightValue
      })
      .unsafeRunSync()
      .value
      .value
      .data should equal("foo".getBytes())
  }

  test("base64 bytes can be received") {
    import base64.client.http4s.Client
    import base64.client.http4s.definitions.Foo
    import base64.client.http4s.Implicits.Base64String
    import org.http4s.dsl._

    def staticClient: Http4sClient[IO] = {
      implicit val fooOkEncoder = jsonEncoderOf[IO, Foo]
      val response = new Http4sDsl[IO] {
        def route: HttpApp[IO] = Kleisli.liftF(Ok(Foo(Some(Base64String("foo".getBytes())))))
      }
      Http4sClient.fromHttpApp[IO](response.route)
    }

    Client
      .httpClient(staticClient, "http://localhost:80")
      .foo()
      .attempt
      .unsafeRunSync()
      .fold(
        _ => fail("Error"),
        _.fold(
          handleOk = _.value.value.data should equal("foo".getBytes())
        )
      )
  }
}
