package generators.Http4s.RoundTrip

import cats.effect.IO
import form.client.{http4s => cdefs}
import form.client.http4s.foo.FooClient
import form.server.{http4s => sdefs}
import form.server.http4s.foo.{DoBarResponse, DoFooResponse, FooHandler, FooResource}
import org.http4s.{Method, Request, Status, Uri, UrlForm}
import org.http4s.client.Client
import org.scalatest.{EitherValues, FunSuite, Matchers}

class Http4sFormDataTest extends FunSuite with Matchers with EitherValues {

  test("present required form param") {
    val fooClient = FooClient.httpClient(Client.fromHttpService(new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] =
        if (status == sdefs.definitions.Status.OK) {
          IO.pure(respond.Ok)
        } else {
          IO.pure(respond.NotAcceptable)
        }
      def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] = ???
    })))
    fooClient.doFoo(cdefs.definitions.Status.OK).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoFooResponse.Ok
  }

  test("missing required form param") {
    val client = Client.fromHttpService(new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] =
        if (status == sdefs.definitions.Status.OK) {
          IO.pure(respond.Ok)
        } else {
          IO.pure(respond.NotAcceptable)
        }
      def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] = ???
    }))
    val req = Request[IO](method = Method.POST, uri = Uri.unsafeFromString("http://localhost:1234/foo")).withBody(UrlForm.empty)
    client.open(req.unsafeRunSync()).unsafeRunSync().response.status shouldBe Status.BadRequest
  }

  test("present optional form param") {
    val fooClient = FooClient.httpClient(Client.fromHttpService(new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] = ???
      def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] =
        if (status.contains(sdefs.definitions.Status.OK)) {
          IO.pure(respond.Ok)
        } else {
          IO.pure(respond.NotAcceptable)
        }
    })))
    fooClient.doBar(Some(cdefs.definitions.Status.OK)).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoBarResponse.Ok
  }

  test("missing optional form param") {
    val fooClient = FooClient.httpClient(Client.fromHttpService(new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] = ???
      def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] =
        if (status.isEmpty) {
          IO.pure(respond.Ok)
        } else {
          IO.pure(respond.NotAcceptable)
        }
    })))
    fooClient.doBar(None).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoBarResponse.Ok
  }
}
