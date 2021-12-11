package core.Http4s

import _root_.customCircePrinter.client.{ http4s => cdefs }
import _root_.customCircePrinter.server.http4s.users.{ UsersHandler, UsersResource }
import _root_.customCircePrinter.server.http4s.users.UsersResource.{ GetUserResponse, PostUserResponse }
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import customCircePrinter.client.http4s.users.UsersClient
import customCircePrinter.server.http4s.definitions.{ User, UserAddress }
import org.http4s.{ HttpRoutes, Request, Uri }
import org.http4s.client.Client
import org.http4s.client.Client
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.syntax.StringSyntax
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import io.circe.{ Json, Printer }

class Http4sCustomCircePrinterTest extends AnyFunSuite with Matchers with EitherValues with StringSyntax {

  test("server: prints nulls with default Printer") {
    val server: HttpRoutes[IO] = new UsersResource[IO]().routes(new UsersHandler[IO] {
      override def getUser(respond: GetUserResponse.type)(id: String): IO[GetUserResponse] =
        IO.pure(GetUserResponse.Ok(User("id", UserAddress(None, Some("line 2"), None))))
      override def postUser(respond: PostUserResponse.type)(body: User): IO[PostUserResponse] =
        IO.pure(PostUserResponse.Ok(""))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved: String =
      client
        .run(Request[IO](uri = Uri.unsafeFromString(s"/user/id")))
        .use { _.bodyText.compile.string }
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual """{"id":"id","address":{"line1":null,"line2":"line 2","line3":null}}"""
  }

  test("server: doesn't print nulls with configured Printer") {
    val printer = Printer.noSpaces.copy(dropNullValues = true)

    val server: HttpRoutes[IO] = new UsersResource[IO](printer = printer).routes(new UsersHandler[IO] {
      override def getUser(respond: GetUserResponse.type)(id: String): IO[GetUserResponse] =
        IO.pure(GetUserResponse.Ok(User("id", UserAddress(None, Some("line 2"), None))))
      override def postUser(respond: PostUserResponse.type)(body: User): IO[PostUserResponse] =
        IO.pure(PostUserResponse.Ok(""))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved: String =
      client
        .run(Request[IO](uri = Uri.unsafeFromString(s"/user/id")))
        .use { _.bodyText.compile.string }
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual """{"id":"id","address":{"line2":"line 2"}}"""
  }

  test("client: prints nulls with default Printer") {
    val server: HttpRoutes[IO] = HttpRoutes.of[IO] {
      case req @ POST -> Root / "user" =>
        req.bodyText.compile.string.map(b => Json.fromString(b).toString()).flatMap(Ok(_))
    }

    val client = UsersClient.httpClient(Client.fromHttpApp(server.orNotFound))

    val user = cdefs.definitions.User("id", cdefs.definitions.UserAddress(None, Some("line 2"), None))

    val retrieved: String =
      client.postUser(user).attempt.unsafeRunSync().value.fold(identity)

    retrieved shouldEqual """{"id":"id","address":{"line1":null,"line2":"line 2","line3":null}}"""
  }

  test("client: doesn't print nulls with configured Printer") {
    val server: HttpRoutes[IO] = HttpRoutes.of[IO] {
      case req @ POST -> Root / "user" =>
        req.bodyText.compile.string.map(b => Json.fromString(b).toString()).flatMap(Ok(_))
    }

    val printer = Printer.noSpaces.copy(dropNullValues = true)
    val client  = UsersClient.httpClient(Client.fromHttpApp(server.orNotFound), printer = printer)

    val user = cdefs.definitions.User("id", cdefs.definitions.UserAddress(None, Some("line 2"), None))

    val retrieved: String =
      client.postUser(user).attempt.unsafeRunSync().value.fold(identity)

    retrieved shouldEqual """{"id":"id","address":{"line2":"line 2"}}"""
  }
}
