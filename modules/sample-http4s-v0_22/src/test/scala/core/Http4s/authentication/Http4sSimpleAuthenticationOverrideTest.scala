package core.Http4s.authentication

import _root_.authenticationOverrideSimple.server.http4sV022.auth.AuthHandler
import _root_.authenticationOverrideSimple.server.http4sV022.auth.AuthResource
import _root_.authenticationOverrideSimple.server.http4sV022.auth.AuthResource._
import cats.data._
import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s.Request
import org.scalatest.funsuite.AnyFunSuite

class Http4sSimpleAuthenticationOverrideTest extends AnyFunSuite {
  type AuthContext = String

  def createServer(authMiddleware: (AuthResource.AuthSchemes, Set[String], Request[IO]) => IO[Either[AuthResource.AuthError, AuthContext]]): HttpRoutes[IO] =
    new AuthResource[IO, AuthContext](authMiddleware).routes(new AuthHandler[IO, AuthContext] {
      override def doBar(respond: DoBarResponse.type)(body: String): IO[DoBarResponse] = ???

      override def doFoo(respond: DoFooResponse.type)(authContext: Either[AuthResource.AuthError, AuthContext], body: String): IO[DoFooResponse] = ???
    })
}
