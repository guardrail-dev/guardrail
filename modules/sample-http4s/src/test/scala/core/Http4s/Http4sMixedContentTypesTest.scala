package core.Http4s

import _root_.mixedContentTypes.client.{ http4s => generatedClient }
import _root_.mixedContentTypes.server.http4s.Resource._
import _root_.mixedContentTypes.server.http4s._
import _root_.mixedContentTypes.server.http4s.definitions.Error
import cats.effect.IO
import cats.effect.IO._
import cats.effect.unsafe.implicits.global
import org.http4s.client.Client
import org.http4s.implicits._
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Http4sMixedContentTypesTest extends AnyFunSuite with Matchers with EitherValues {

  test("handle text/plain") {
    val server = new Resource[IO]().routes(new Handler[IO] {
      override def doFoo(respond: DoFooResponse.type)(): IO[DoFooResponse] =
        IO.pure(respond.Ok("resp"))
    })

    val client = generatedClient.Client.httpClient(Client.fromHttpApp(server.orNotFound), "http://localhost:1234")

    val actual = client.doFoo(Nil).attempt.unsafeRunSync().value
    actual shouldBe generatedClient.DoFooResponse.Ok("resp")
  }

  test("handle application/json") {
    val server = new Resource[IO]().routes(new Handler[IO] {
      override def doFoo(respond: DoFooResponse.type)(): IO[DoFooResponse] =
        IO.pure(respond.BadRequest(Error(Some("err"))))
    })

    val client = generatedClient.Client.httpClient(Client.fromHttpApp(server.orNotFound), "http://localhost:1234")

    val actual = client.doFoo(Nil).attempt.unsafeRunSync().value
    actual shouldBe generatedClient.DoFooResponse.BadRequest(generatedClient.definitions.Error(Some("err")))
  }

}
