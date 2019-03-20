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

class DropwizardRoundTripTest extends FreeSpec with Matchers with Waiters with MockitoSugar with ArgumentMatchersSugar {
  private implicit val mapper = new ObjectMapper

  "Test server" in {
    val USERNAME = "foobar"

    val (asyncResponse, serverFuture: CompletableFuture[GetUserByNameResponse]) = mockAsyncResponse[GetUserByNameResponse]

    val resource = new UserResource(new UserHandler {
      override def createUser(body: User): CompletionStage[CreateUserResponse] = ???
      override def createUsersWithArrayInput(body: util.List[User]): CompletionStage[CreateUsersWithArrayInputResponse] = ???
      override def createUsersWithListInput(body: util.List[User]): CompletionStage[CreateUsersWithListInputResponse] = ???
      override def loginUser(username: String, password: String): CompletionStage[LoginUserResponse] = ???
      override def logoutUser(): CompletionStage[LogoutUserResponse] = ???
      override def updateUser(username: String, body: User): CompletionStage[UpdateUserResponse] = ???
      override def deleteUser(username: String): CompletionStage[DeleteUserResponse] = ???

      override def getUserByName(username: String): CompletionStage[GetUserByNameResponse] = {
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
          serverFuture.thenApply({ response => response match {
            case r: GetUserByNameResponse.Ok => mockAHCResponse(request.getUrl, r.getStatusCode, Some(r.getValue))
            case r: GetUserByNameResponse.BadRequest => mockAHCResponse(request.getUrl, r.getStatusCode)
            case r: GetUserByNameResponse.NotFound => mockAHCResponse(request.getUrl, r.getStatusCode)
          }})
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
