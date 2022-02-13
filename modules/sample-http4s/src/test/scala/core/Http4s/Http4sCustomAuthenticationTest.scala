package core.Http4s

import _root_.authenticationCustom.client.{ http4s => cdefs }
import _root_.authenticationCustom.server.http4s.auth.AuthHandler
import _root_.authenticationCustom.server.http4s.auth.AuthResource
import _root_.authenticationCustom.server.http4s.auth.AuthResource._
import authenticationCustom.client.http4s.auth.AuthClient
import cats.data._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
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

class Http4sCustomAuthenticationTest extends AnyFunSuite with Matchers with EitherValues with StringSyntax {

  test("provide context to handler") {
    type AuthContext = Int
    type AuthError   = Unit

    val authMiddleware = (_: NonEmptyList[NonEmptyMap[AuthResource.AuthSchemes, Set[String]]], _: Request[IO]) => IO.pure(Right(42))

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext, AuthError](authMiddleware).routes(new AuthHandler[IO, AuthContext, AuthError] {
      override def doBar(respond: DoBarResponse.type)(body: String): IO[DoBarResponse] = ???
      override def doFoo(respond: DoFooResponse.type)(authContext: Either[AuthError, AuthContext], body: String): IO[DoFooResponse] =
        authContext.fold(_ => IO(DoFooResponse.Ok("authentication failed")), ctx => IO(DoFooResponse.Ok(ctx.toString() + body)))
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
    type AuthError   = Unit

    val authMiddleware = (_: NonEmptyList[NonEmptyMap[AuthResource.AuthSchemes, Set[String]]], _: Request[IO]) => IO.pure(Left(()))

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext, AuthError](authMiddleware).routes(new AuthHandler[IO, AuthContext, AuthError] {
      override def doBar(respond: DoBarResponse.type)(body: String): IO[DoBarResponse] = ???
      override def doFoo(respond: DoFooResponse.type)(authContext: Either[AuthError, AuthContext], body: String): IO[DoFooResponse] =
        authContext.fold(_ => IO(DoFooResponse.Ok("authentication failed")), ctx => IO(DoFooResponse.Ok(ctx.toString() + body)))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved =
      client
        .run(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .use(_.bodyText.compile.string)
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual "\"authentication failed\""
  }
  test("provide security requirements to authentication") {
    type AuthContext = Int
    type AuthError   = Unit
    import AuthResource.AuthSchemes

    val authMiddleware = (config: NonEmptyList[NonEmptyMap[AuthSchemes, Set[String]]], _: Request[IO]) =>
      IO.pure {
        val c = config.toList.map(_.toSortedMap.toMap)

        if (c == List(Map(AuthSchemes.Jwt -> Set("foo:read", "bar:write")), Map(AuthSchemes.OAuth2 -> Set("oauth:scope"))))
          Right(1)
        else
          Left(())
      }

    val server: HttpRoutes[IO] = new AuthResource[IO, AuthContext, AuthError](authMiddleware).routes(new AuthHandler[IO, AuthContext, AuthError] {
      override def doBar(respond: DoBarResponse.type)(body: String): IO[DoBarResponse] = ???
      override def doFoo(respond: DoFooResponse.type)(authContext: Either[AuthError, AuthContext], body: String): IO[DoFooResponse] =
        authContext.fold(_ => IO(DoFooResponse.Ok("test failed")), ctx => IO(DoFooResponse.Ok("test succeed")))
    })

    val client = Client.fromHttpApp(server.orNotFound)

    val retrieved =
      client
        .run(
          Request[IO](method = Method.POST, uri = Uri.unsafeFromString("/foo"))
            .withBodyStream(fs2.Stream.apply("\"\"".getBytes(): _*))
            .withContentType(`Content-Type`(MediaType.application.json))
        )
        .use(_.bodyText.compile.string)
        .attempt
        .unsafeRunSync()
        .value

    retrieved shouldEqual "\"test succeed\""
  }
}
