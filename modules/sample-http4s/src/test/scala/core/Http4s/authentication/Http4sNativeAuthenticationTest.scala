package core.Http4s.authentication

import _root_.authenticationNative.client.{ http4s => cdefs }
import _root_.authenticationNative.server.http4s.auth.AuthHandler
import _root_.authenticationNative.server.http4s.auth.AuthResource
import _root_.authenticationNative.server.http4s.auth.AuthResource._
import authenticationCustom.client.http4s.auth.AuthClient
import cats.data._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import io.circe.Json
import io.circe.Printer
import org.http4s.AuthedRoutes
import org.http4s.BasicCredentials
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.Method
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.headers.Authorization
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.server.middleware.authentication.BasicAuth
import org.http4s.syntax.StringSyntax
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Http4sNativeAuthenticationTest extends AnyFunSuite with Matchers with EitherValues with StringSyntax {
  def prepareServer[AuthContext](authResult: AuthContext): HttpRoutes[IO] = {
    val server: AuthedRoutes[AuthContext, IO] = new AuthResource[IO, AuthContext]().routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(authContext: AuthContext, p1: String, body: String): IO[DoBarResponse] = ???

      override def doBaz(respond: DoBazResponse.type)(authContext: AuthContext, body: String): IO[DoBazResponse] = ???

      override def doFoo(respond: DoFooResponse.type)(authContext: AuthContext, body: String): IO[DoFooResponse] =
        IO(respond.Ok(authContext.toString() + body))
    })

    val authMiddleware = BasicAuth[IO, AuthContext](
      "http4s tests",
      {
        case BasicCredentials("good", "login", _) => Option(authResult).pure[IO]
        case _                                    => Option.empty[AuthContext].pure[IO]
      }
    )

    authMiddleware(server)
  }

  def getFoo[A](server: HttpRoutes[IO], transformRequest: Request[IO] => Request[IO] = identity _)(extract: Response[IO] => IO[A]): A = {
    val client = Client.fromHttpApp(server.orNotFound)

    client
      .run(
        transformRequest(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"\"".getBytes().toIndexedSeq: _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
      )
      .use(extract)
      .attempt
      .unsafeRunSync()
      .value
  }

  test("process authentication error") {
    val (code, body) = getFoo(prepareServer(42)) { resp =>
      for {
        body <- resp.bodyText.compile.string
      } yield (resp.status.code, body)
    }

    code shouldEqual 401
    body shouldEqual ""
  }

  test("process authentication success") {
    val (code, body) = getFoo(prepareServer(42), _.putHeaders(Authorization(BasicCredentials("good", "login")))) { resp =>
      for {
        body <- resp.bodyText.compile.string
      } yield (resp.status.code, body)
    }

    code shouldEqual 200
    body shouldEqual "\"42\""
  }
}
