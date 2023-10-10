package core.issues

import scala.language.reflectiveCalls

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
import org.scalatest.EitherValues
import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import tests.scalatest.EitherTValues

class Issue542Suite extends AnyFunSuite with Matchers with EitherValues with ScalaFutures with EitherTValues with OptionValues {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  test("base64 bytes can be sent") {
    import base64.server.http4sV022.{ Handler, Resource }
    import base64.server.http4sV022.Resource.FooResponse
    import base64.server.http4sV022.definitions.Foo
    import base64.server.http4sV022.Implicits.Base64String

    val route = new Resource[IO]().routes(new Handler[IO] {
      def foo(respond: FooResponse.type)(): IO[FooResponse] = IO.pure(respond.Ok(Foo(Some(new Base64String("foo".getBytes())))))
    })

    val client = Http4sClient.fromHttpApp[IO](route.orNotFound)

    val req = Request[IO](method = Method.GET, uri = Uri.unsafeFromString("/foo"))

    client
      .run(req)
      .use { case Status.Ok(resp) =>
        resp.status should equal(Status.Ok)
        resp.contentType should equal(Some(`Content-Type`(MediaType.application.json)))
        resp.contentLength should equal(Some(16))
        jsonOf[IO, Foo].decode(resp, strict = false).rightValue
      }
      .unsafeRunSync()
      .value
      .value
      .data should equal("foo".getBytes())
  }

  test("base64 bytes can be received") {
    import base64.client.http4sV022.Client
    import base64.client.http4sV022.definitions.Foo
    import base64.client.http4sV022.Implicits.Base64String
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
