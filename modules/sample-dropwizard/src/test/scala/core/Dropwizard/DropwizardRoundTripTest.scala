package core.Dropwizard

import com.fasterxml.jackson.databind.ObjectMapper
import examples.client.dropwizard.user.UserClient
import examples.server.dropwizard.definitions.User
import examples.server.dropwizard.user._
import helpers.MockHelpers._
import java.util
import java.util.Optional
import java.util.concurrent.{CompletableFuture, CompletionStage}
import org.asynchttpclient.{Request, Response}
import org.mockito.{ArgumentMatchersSugar, MockitoSugar}
import org.scalatest.concurrent.Waiters
import org.scalatest.{FreeSpec, Matchers}
import scala.compat.java8.FunctionConverters._
import scala.compat.java8.OptionConverters._

class DropwizardRoundTripTest extends FreeSpec with Matchers with Waiters with MockitoSugar with ArgumentMatchersSugar {
  private implicit val mapper = new ObjectMapper

  "Test server" in {
    val USERNAME = "foobar"

    val serverFuture = new CompletableFuture[GetUserByNameResponse[_]]
    val asyncResponse = mockAsyncResponse(serverFuture)

    val resource = new UserResource(new UserHandler {
      override def createUser(body: User): CompletionStage[CreateUserResponse[_]] = ???
      override def createUsersWithArrayInput(body: util.List[User]): CompletionStage[CreateUsersWithArrayInputResponse[_]] = ???
      override def createUsersWithListInput(body: util.List[User]): CompletionStage[CreateUsersWithListInputResponse[_]] = ???
      override def loginUser(username: String, password: String): CompletionStage[LoginUserResponse[_]] = ???
      override def logoutUser(): CompletionStage[LogoutUserResponse[_]] = ???
      override def updateUser(username: String, body: User): CompletionStage[UpdateUserResponse[_]] = ???
      override def deleteUser(username: String): CompletionStage[DeleteUserResponse[_]] = ???

      override def getUserByName(username: String): CompletionStage[GetUserByNameResponse[_]] = {
        username match {
          case USERNAME =>
            serverFuture.complete(GetUserByNameResponse.Ok(User.builder()
              .withEmail("foo@bar.com")
              .withFirstName("Foo")
              .withLastName("Bar")
              .withId(1)
              .withUsername(USERNAME)
              .build()
            ))
          case "" =>
            serverFuture.complete(GetUserByNameResponse.BadRequest)
          case _ =>
            serverFuture.complete(GetUserByNameResponse.NotFound)
        }
        serverFuture
      }
    })

    val httpClient: Request => CompletionStage[Response] = { request =>
      val userPath = "^/v2/user/([^/]*)$".r
      request.getUri.getPath match {
        case userPath(username) =>
          resource.getUserByName(username, asyncResponse)
          serverFuture.thenApply(response => mockAHCResponse(request.getUrl, response.getStatusCode, response.getEntityBody.asScala))
        case _ =>
          CompletableFuture.completedFuture(mockAHCResponse(request.getUrl, 404))
      }
    }

    val client = new UserClient.Builder()
      .withHttpClient(httpClient.asJava)
      .withObjectMapper(mapper)
      .build()

    val w = new Waiter
    client.getUserByName(USERNAME).call().whenComplete({ (response, t) =>
      w { t shouldBe null }
      response match {
        case r: UserClient.GetUserByNameResponse.Ok =>
          w {
            r.getValue.getUsername.get shouldBe USERNAME
            r.getValue.getPassword shouldBe Optional.empty
          }
        case _: UserClient.GetUserByNameResponse.BadRequest => w { fail("Got BadRequest") }
        case _: UserClient.GetUserByNameResponse.NotFound => w { fail("Got NotFound") }
      }
      w.dismiss()
    })
    w.await(dismissals(1))
  }
}
