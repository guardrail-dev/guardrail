package generators.Http4s.Client.contentType

import _root_.tests.contentTypes.textPlain.client.http4s.foo.FooClient
import _root_.tests.contentTypes.textPlain.client.{ http4s => cdefs }
import _root_.tests.contentTypes.textPlain.server.http4s.foo.{ DoBarResponse, DoBazResponse, DoFooResponse, FooHandler, FooResource }
import _root_.tests.contentTypes.textPlain.server.{ http4s => sdefs }
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.http4s.dsl.io._
import org.http4s.headers._
import cats.effect.IO
import org.http4s.client.Client
import org.http4s.{ Charset, HttpRoutes, MediaType }

class Http4sTextPlainTest extends AnyFunSuite with Matchers with EitherValues {
  import org.http4s.implicits._
  test("Plain text should be emitted for required parameters (raw)") {
    val route: HttpRoutes[IO] = HttpRoutes.of {
      case req @ POST -> Root / "foo" =>
        if (req.contentType.contains(`Content-Type`(MediaType.text.plain, Charset.`UTF-8`))) {
          for {
            value <- req.as[String]
            resp  <- if (value == "sample") Created("response") else NotAcceptable()
          } yield resp
        } else NotAcceptable()
    }
    val client: Client[IO] = Client.fromHttpApp(route.orNotFound)
    val fooClient          = FooClient.httpClient(client)
    fooClient.doFoo("sample").attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoFooResponse.Created("response")
  }

  test("Plain text should be emitted for optional parameters (raw)") {
    val route: HttpRoutes[IO] = HttpRoutes.of {
      case req @ POST -> Root / "bar" =>
        if (req.contentType.contains(`Content-Type`(MediaType.text.plain, Charset.`UTF-8`))) {
          for {
            value <- req.as[String]
            resp  <- if (value == "sample") Created("response") else NotAcceptable()
          } yield resp
        } else NotAcceptable()
    }
    val client: Client[IO] = Client.fromHttpApp(route.orNotFound)
    val fooClient          = FooClient.httpClient(client)
    fooClient.doBar(Some("sample")).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoBarResponse.Created("response")
  }

  test("Plain text should be emitted for required parameters") {
    val route: HttpRoutes[IO] = new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(respond: DoFooResponse.type)(body: String): IO[sdefs.foo.DoFooResponse] =
        if (body == "sample") {
          IO.pure(respond.Created("response"))
        } else {
          IO.pure(respond.NotAcceptable)
        }
      def doBar(respond: DoBarResponse.type)(body: Option[String]): IO[sdefs.foo.DoBarResponse] = ???
      def doBaz(respond: DoBazResponse.type)(body: Option[String]): IO[sdefs.foo.DoBazResponse] = ???
    })

    val client: Client[IO] = Client.fromHttpApp(route.orNotFound)
    val fooClient          = FooClient.httpClient(client)
    fooClient.doFoo("sample").attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoFooResponse.Created("response")
  }

  test("Plain text should be emitted for present optional parameters") {
    val route: HttpRoutes[IO] = new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(respond: DoFooResponse.type)(body: String): IO[sdefs.foo.DoFooResponse] = ???
      def doBar(respond: DoBarResponse.type)(body: Option[String]): IO[sdefs.foo.DoBarResponse] =
        if (body.contains("sample")) {
          IO.pure(respond.Created("response"))
        } else {
          IO.pure(respond.NotAcceptable)
        }
      def doBaz(respond: DoBazResponse.type)(body: Option[String]): IO[sdefs.foo.DoBazResponse] = ???
    })

    val client: Client[IO] = Client.fromHttpApp(route.orNotFound)
    val fooClient          = FooClient.httpClient(client)
    fooClient.doBar(Some("sample")).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoBarResponse.Created("response")
  }

  test("Plain text should be emitted for missing optional parameters") {
    val route: HttpRoutes[IO] = new FooResource[IO]().routes(new FooHandler[IO] {
      def doFoo(respond: DoFooResponse.type)(body: String): IO[sdefs.foo.DoFooResponse] = ???
      def doBar(respond: DoBarResponse.type)(body: Option[String]): IO[sdefs.foo.DoBarResponse] =
        if (body.isEmpty) {
          IO.pure(respond.Created("response"))
        } else {
          IO.pure(respond.NotAcceptable)
        }
      def doBaz(respond: DoBazResponse.type)(body: Option[String]): IO[sdefs.foo.DoBazResponse] = ???
    })

    val client: Client[IO] = Client.fromHttpApp(route.orNotFound)
    val fooClient          = FooClient.httpClient(client)
    fooClient.doBar(None).attempt.unsafeRunSync().right.value shouldBe cdefs.foo.DoBarResponse.Created("response")
  }
}
