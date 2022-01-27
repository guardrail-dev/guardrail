package core.Http4s

import _root_.customExtraction.client.{ http4s => cdefs }
import _root_.customExtraction.server.http4s.users.{ UsersHandler, UsersResource }
import _root_.customExtraction.server.http4s.users.UsersResource.GetUserResponse
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import customExtraction.client.http4s.users.UsersClient
import customExtraction.server.http4s.definitions.{ User, UserAddress }
import org.http4s.{ HttpRoutes, Request }
import org.http4s.client.Client
import org.http4s.implicits._
import org.http4s.syntax.StringSyntax
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Http4sCustomExtractorTest extends AnyFunSuite with Matchers with EitherValues with StringSyntax {
  type Extract = IO[String]
  private val testString = "test"
  private val user       = User("id", UserAddress(Some("line1"), Some("line2"), Some("line3")))

  def extract: String => Request[IO] => Extract = { name => request =>
    IO.pure(testString)
  }

  test("custom extract: injecting value into handler") {
    val server: HttpRoutes[IO] = new UsersResource[IO, Extract](extract).routes(new UsersHandler[IO, Extract] {
      override def getUser(respond: GetUserResponse.type)(id: String)(extracted: Extract): IO[GetUserResponse] =
        extracted.map { str =>
          if (str == testString) GetUserResponse.Ok(user)
          else GetUserResponse.Ok(User("baduser", UserAddress(None, None, None)))
        }
    })

    val client = UsersClient.httpClient(Client.fromHttpApp(server.orNotFound))

    val retrieved: cdefs.users.GetUserResponse =
      client.getUser(user.id, List.empty).attempt.unsafeRunSync().value

    retrieved shouldBe cdefs.users.GetUserResponse
      .Ok(cdefs.definitions.User("id", cdefs.definitions.UserAddress(Some("line1"), Some("line2"), Some("line3"))))
  }
}
