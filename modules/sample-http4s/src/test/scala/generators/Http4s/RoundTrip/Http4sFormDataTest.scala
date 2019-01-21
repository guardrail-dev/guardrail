package generators.Http4s.RoundTrip

import cats.effect.IO
import form.client.{ http4s => cdefs }
import form.client.http4s.foo.FooClient
import form.server.{ http4s => sdefs }
import form.server.http4s.foo.{ DoBarResponse, DoFooResponse, FooHandler, FooResource }
import org.http4s.{ Method, Request, Status, Uri, UrlForm }
import org.http4s.client.Client
import org.http4s.implicits._
import org.scalatest.{ EitherValues, FunSuite, Matchers }

class Http4sFormDataTest extends FunSuite with Matchers with EitherValues {

  test("present required form param") {
    val fooClient = FooClient.httpClient(
      Client.fromHttpApp(
        new FooResource[IO]()
          .routes(new FooHandler[IO] {
            def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] =
              if (status == sdefs.definitions.Status.OK) {
                IO.pure(respond.Ok)
              } else {
                IO.pure(respond.NotAcceptable)
              }
            def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] = ???
          })
          .orNotFound
      )
    )
    fooClient.doFoo(cdefs.definitions.Status.OK).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoFooResponse.Ok
  }

  test("missing required form param") {
    val client = Client.fromHttpApp(
      new FooResource[IO]()
        .routes(new FooHandler[IO] {
          def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] =
            if (status == sdefs.definitions.Status.OK) {
              IO.pure(respond.Ok)
            } else {
              IO.pure(respond.NotAcceptable)
            }
          def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] = ???
        })
        .orNotFound
    )
    val req = Request[IO](method = Method.POST, uri = Uri.unsafeFromString("http://localhost:1234/foo")).withEntity(UrlForm.empty)
    client.run(req).use(r => IO(r.status)).unsafeRunSync() shouldBe Status.BadRequest
  }

  test("present optional form param") {
    val fooClient = FooClient.httpClient(
      Client.fromHttpApp(
        new FooResource[IO]()
          .routes(new FooHandler[IO] {
            def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] = ???
            def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] =
              if (status.contains(sdefs.definitions.Status.OK)) {
                IO.pure(respond.Ok)
              } else {
                IO.pure(respond.NotAcceptable)
              }
          })
          .orNotFound
      )
    )
    fooClient.doBar(Some(cdefs.definitions.Status.OK)).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoBarResponse.Ok
  }

  test("missing optional form param") {
    val fooClient = FooClient.httpClient(
      Client.fromHttpApp(
        new FooResource[IO]()
          .routes(new FooHandler[IO] {
            def doFoo(respond: DoFooResponse.type)(status: sdefs.definitions.Status): IO[DoFooResponse] = ???
            def doBar(respond: DoBarResponse.type)(status: Option[sdefs.definitions.Status]): IO[DoBarResponse] =
              if (status.isEmpty) {
                IO.pure(respond.Ok)
              } else {
                IO.pure(respond.NotAcceptable)
              }
          })
          .orNotFound
      )
    )
    fooClient.doBar(None).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoBarResponse.Ok
  }
}
