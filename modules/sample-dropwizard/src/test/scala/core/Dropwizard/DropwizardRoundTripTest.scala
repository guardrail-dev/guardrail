package core.Dropwizard

import alias.client.dropwizard.foo.FooClient
import alias.server.dropwizard.foo.{DoFooResponse, FooHandler, FooResource}
import com.fasterxml.jackson.databind.ObjectMapper
import examples.server.dropwizard.definitions.User
import examples.server.dropwizard.user._
import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.util
import java.util.Optional
import java.util.concurrent.{CompletableFuture, CompletionStage}
import javax.ws.rs.container.AsyncResponse
import org.asynchttpclient.{Request, Response}
import org.mockito.{ArgumentMatchersSugar, MockitoSugar}
import org.scalatest.concurrent.Waiters
import org.scalatest.{FreeSpec, Matchers}
import scala.collection.JavaConverters._
import scala.compat.java8.FunctionConverters._
import scala.compat.java8.OptionConverters._
import scala.reflect.ClassTag
import scala.util.Try

class DropwizardRoundTripTest extends FreeSpec with Matchers with Waiters with MockitoSugar with ArgumentMatchersSugar {
  private val mapper = new ObjectMapper

  private def mockAsyncResponse[T](implicit cls: ClassTag[T]): (AsyncResponse, CompletableFuture[T]) = {
    val asyncResponse = mock[AsyncResponse]
    val future = new CompletableFuture[T]

    when(asyncResponse.resume(any[T])) thenAnswer[AnyRef] { response => response match {
      case t: Throwable => future.completeExceptionally(t)
      case other: T => future.complete(other)
      case other => fail(s"AsyncResponse.resume expected an object of type ${cls.runtimeClass.getName}, but got ${other.getClass.getName} instead")
    }}

    (asyncResponse, future)
  }

  "Test server" in {
    val USERNAME = "foobar"

    val (asyncResponse, future) = mockAsyncResponse[GetUserByNameResponse]

    val resource = new UserResource(new UserHandler {
      override def createUser(body: User): CompletionStage[CreateUserResponse] = ???
      override def createUsersWithArrayInput(body: util.List[User]): CompletionStage[CreateUsersWithArrayInputResponse] = ???
      override def createUsersWithListInput(body: util.List[User]): CompletionStage[CreateUsersWithListInputResponse] = ???
      override def loginUser(username: String, password: String): CompletionStage[LoginUserResponse] = ???
      override def logoutUser(): CompletionStage[LogoutUserResponse] = ???
      override def updateUser(username: String, body: User): CompletionStage[UpdateUserResponse] = ???
      override def deleteUser(username: String): CompletionStage[DeleteUserResponse] = ???

      override def getUserByName(username: String): CompletionStage[GetUserByNameResponse] = {
        if (USERNAME == "foobar") {
          future.complete(new GetUserByNameResponse.Ok(User.b))
        }
      }
    })
    val resource = new FooResource(new FooHandler {
      override def doFoo(long: Optional[java.lang.Long], body: Optional[java.lang.Long]): CompletionStage[DoFooResponse] = {
        future.complete(new DoFooResponse.Created(long.orElse(42L)))
        future
      }
    })

    val httpClient: Request => CompletionStage[Response] = { request =>
      if (request.getUri.getPath == "/foo") {
        println("got request")
        val queryParams = request.getQueryParams.asScala
        val long = queryParams.find(_.getName == "long").map[java.lang.Long](_.getValue.toLong).asJava
        val body = Try(java.lang.Long.valueOf(new String(request.getByteData, request.getCharset))).toOption.asJava
        resource.doFoo(long, body, asyncResponse)
        future.thenApply({
          case _: DoFooResponse.Created =>
            println("applying response object to http response")
            val response = mock[Response]
            when(response.getStatusCode) thenReturn 201
            when(response.getResponseBody(any)) thenReturn ""
            when(response.getResponseBody) thenReturn ""
            when(response.getResponseBodyAsStream) thenReturn new ByteArrayInputStream(new Array[Byte](0))
            when(response.getResponseBodyAsByteBuffer) thenReturn ByteBuffer.allocate(0)
            response
        })
      } else {
        CompletableFuture.completedFuture(new Response.ResponseBuilder().build())
      }
    }

    val client = new FooClient.Builder()
      .withHttpClient(httpClient.asJava)
      .withObjectMapper(mapper)
      .build()

    val w = new Waiter
    client.doFoo(Optional.of(42), Optional.of(99)).whenComplete({ (response, t) =>
      w {
        t shouldBe null
        response.getClass shouldBe classOf[FooClient.DoFooResponse.Created]
      }
      w.dismiss()
    })
    w.await(dismissals(1))
  }
}
