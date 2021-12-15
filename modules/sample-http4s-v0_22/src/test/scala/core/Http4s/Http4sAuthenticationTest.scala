package core.Http4s

import _root_.authentication.client.{ http4sV022 => cdefs }
import _root_.authentication.server.http4sV022.auth.AuthHandler
import _root_.authentication.server.http4sV022.auth.AuthResource
import _root_.authentication.server.http4sV022.auth.AuthResource.DoBarResponse
import _root_.authentication.server.http4sV022.auth.AuthResource.DoFooResponse
import authentication.client.http4sV022.auth.AuthClient
import cats.effect.IO
import io.circe.Json
import io.circe.Printer
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.syntax.StringSyntax
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Http4sAuthenticationTest extends AnyFunSuite with Matchers with EitherValues with StringSyntax {

  test("provide context to handler") {
    type AuthContext = Int

    val authMiddleware = (req: Request[IO], handle: (AuthContext => IO[Response[IO]])) => handle(42)

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext](authMiddleware).routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(body: String): IO[DoBarResponse] = ???
      override def doFoo(respond: DoFooResponse.type)(authContext: AuthContext, body: String): IO[DoFooResponse] =
        IO(DoFooResponse.Ok(authContext.toString() + body))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved =
      client
        .run(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"-97-\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .use(_.bodyText.compile.string)
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual "\"42-97-\""
  }

  test("return response directly from authentication") {
    type AuthContext = Int

    val authMiddleware = (req: Request[IO], handle: (AuthContext => IO[Response[IO]])) => IO(Response[IO](Status.Forbidden))

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext](authMiddleware).routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(body: String): IO[DoBarResponse]                           = ???
      override def doFoo(respond: DoFooResponse.type)(authContext: AuthContext, body: String): IO[DoFooResponse] = ???
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved =
      client
        .status(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual Status.Forbidden
  }
}
