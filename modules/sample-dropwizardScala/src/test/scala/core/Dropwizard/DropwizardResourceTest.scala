package core.Dropwizard

import cats.implicits._
import examples.server.dropwizardScala.definitions.User
import examples.server.dropwizardScala.user.{ UserHandler, UserResource }
import helpers.ScalaHelpers.ResourceTestSupportBuilderExtensions
import helpers.{ ResourceTestSupport, ScalaHelpers }
import io.dropwizard.testing.common.Resource
import javax.ws.rs.client.Entity
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory
import org.mockito.{ ArgumentMatchersSugar, IdiomaticMockito }
import org.scalatest.BeforeAndAfter
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DropwizardResourceTest {
  private val USERNAME = "foobar"
  private val PASSWORD = "sekrit"
  private val TOKEN    = "abc123"
  private val USER     = User(id = Some(1), username = Some(USERNAME), password = Some(PASSWORD), firstName = Some("Blast"), lastName = Some("Hardcheese"))

  private class UserHandlerImpl extends UserHandler {
    override def createUser(respond: UserResource.CreateUserResponse.type)(body: User)                                       = respond.OK.pure[Future]
    override def createUsersWithArrayInput(respond: UserResource.CreateUsersWithArrayInputResponse.type)(body: Vector[User]) = respond.OK.pure[Future]
    override def createUsersWithListInput(respond: UserResource.CreateUsersWithListInputResponse.type)(body: Vector[User])   = respond.OK.pure[Future]
    override def loginUser(respond: UserResource.LoginUserResponse.type)(username: String, password: String) =
      (username, password) match {
        case (USERNAME, PASSWORD) => respond.OK(TOKEN).pure[Future]
        case _                    => respond.BadRequest.pure[Future]
      }
    override def logoutUser(respond: UserResource.LogoutUserResponse.type)() = respond.OK.pure[Future]
    override def getUserByName(respond: UserResource.GetUserByNameResponse.type)(username: String) = username match {
      case USERNAME => respond.OK(USER).pure[Future]
      case " "      => respond.BadRequest.pure[Future]
      case _        => respond.NotFound.pure[Future]
    }
    override def updateUser(respond: UserResource.UpdateUserResponse.type)(username: String, body: User) = respond.BadRequest.pure[Future]
    override def deleteUser(respond: UserResource.DeleteUserResponse.type)(username: String)             = respond.NotFound.pure[Future]
  }
}

class DropwizardResourceTest extends AnyFreeSpec with ResourceTestSupport with BeforeAndAfter with IdiomaticMockito with ArgumentMatchersSugar with Matchers {
  import DropwizardResourceTest._

  private val userHandler = spy(new UserHandlerImpl)

  override def buildResource: Resource =
    ResourceTestSupport
      .Builder()
      .setTestContainerFactory(new GrizzlyTestContainerFactory)
      .setMapper(ScalaHelpers.createObjectMapper())
      .addScalaProviders()
      .addResource(new UserResource(userHandler))
      .build()

  before {
    reset(userHandler)
  }

  "getUserByName should" - {
    "return 200" in {
      resource.target("/v2/user/" + USERNAME).request.get(classOf[User]) mustBe USER
      userHandler.getUserByName(any)(USERNAME) was called
    }

    "return 404 for an unknown user" in {
      resource.target("/v2/user/nope").request.build("GET").invoke.getStatus mustBe 404
    }

    "return 400 for an invalid username" in {
      resource.target("/v2/user/ ").request.build("GET").invoke.getStatus mustBe 400
    }
  }

  "createUser should" - {
    "return 200" in {
      resource.target("/v2/user").request.post(Entity.json(USER)).getStatus mustBe 200
      userHandler.createUser(any)(USER) was called
    }

    "return 422 on missing request body" in {
      resource.target("/v2/user").request.build("POST").invoke.getStatus mustBe 422
      userHandler.createUser(any)(any) wasNever called
    }
  }

  "loginUser should" - {
    "return 200" in {
      resource
        .target("/v2/user/login")
        .queryParam("username", USERNAME)
        .queryParam("password", PASSWORD)
        .request
        .get(classOf[String]) mustBe TOKEN
      userHandler.loginUser(any)(USERNAME, PASSWORD) was called
    }

    "return 400 on a bad password" in {
      resource
        .target("/v2/user/login")
        .queryParam("username", "moo")
        .queryParam("password", "")
        .request
        .build("GET")
        .invoke
        .getStatus mustBe 400
    }
  }
}
