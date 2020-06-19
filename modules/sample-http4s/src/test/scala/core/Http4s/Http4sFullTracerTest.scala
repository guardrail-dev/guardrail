package core.Http4s

import _root_.tracer.client.{ http4s => cdefs }
import _root_.tracer.server.http4s.addresses.{ AddressesHandler, AddressesResource, GetAddressResponse, GetAddressesResponse }
import _root_.tracer.server.http4s.users.{ GetUserResponse, UsersHandler, UsersResource }
import _root_.tracer.server.{ http4s => sdefs }
import _root_.tracer.client.http4s.users.UsersClient
import _root_.tracer.client.http4s.addresses.AddressesClient
import _root_.tracer.server.http4s.Http4sImplicits.TraceBuilder
import cats.effect.IO
import org.http4s.{ Header, HttpRoutes, Request }
import org.http4s.client.Client
import org.http4s.implicits._
import org.http4s.syntax.StringSyntax
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Http4sFullTracerTest extends AnyFunSuite with Matchers with EitherValues with StringSyntax {

  val traceHeaderKey          = "tracer-label"
  def log(line: String): Unit = ()

  def trace: String => Request[IO] => TraceBuilder[IO] = { name => request =>
    // In a real environment, this would be where you could establish a new
    // tracing context and inject that fresh header value.
    log(s"Expecting all requests to have ${traceHeaderKey} header.")
    traceBuilder(request.headers.get(traceHeaderKey.ci).get.value)
  }

  def traceBuilder(parentValue: String): TraceBuilder[IO] = { name => httpClient =>
    Client { req =>
      httpClient.run(req.putHeaders(Header(traceHeaderKey, parentValue)))
    }
  }

  test("full tracer: passing headers through multiple levels") {
    // Establish the "Address" server
    val server2: HttpRoutes[IO] =
      new AddressesResource(trace).routes(
        new AddressesHandler[IO] {
          def getAddress(respond: GetAddressResponse.type)(id: String)(traceBuilder: TraceBuilder[IO]) =
            IO.pure(if (id == "addressId") {
              respond.Ok(sdefs.definitions.Address(Some("line1"), Some("line2"), Some("line3")))
            } else sdefs.addresses.GetAddressResponse.NotFound)
          def getAddresses(respond: GetAddressesResponse.type)()(traceBuilder: TraceBuilder[IO]) =
            IO.pure(sdefs.addresses.GetAddressesResponse.NotFound)
        }
      )

    // Establish the "User" server
    val server1: HttpRoutes[IO] =
      new UsersResource(trace).routes(
        new UsersHandler[IO] {
          // ... using the "Address" server explicitly in the addressesClient
          val addressesClient = AddressesClient.httpClient(Client.fromHttpApp(server2.orNotFound))
          def getUser(respond: GetUserResponse.type)(id: String)(traceBuilder: TraceBuilder[IO]) =
            addressesClient
              .getAddress(traceBuilder, "addressId")
              .map {
                case cdefs.addresses.GetAddressResponse.Ok(address) =>
                  respond.Ok(sdefs.definitions.User("1234", sdefs.definitions.UserAddress(address.line1, address.line2, address.line3)))
                case cdefs.addresses.GetAddressResponse.NotFound => respond.NotFound
              }
        }
      )

    // Build a UsersClient using the User server
    val usersClient = UsersClient.httpClient(Client.fromHttpApp(server1.orNotFound))
    // As this is the entry point, we either have a tracing header from
    // somewhere else, or we generate one for top-level request.
    val testTrace = traceBuilder("top-level-request")

    // Make a request against the mock servers using a hard-coded user ID
    val retrieved: cdefs.users.GetUserResponse = usersClient.getUser(testTrace, "1234").attempt.unsafeRunSync().right.value

    retrieved shouldBe cdefs.users.GetUserResponse
      .Ok(cdefs.definitions.User("1234", cdefs.definitions.UserAddress(Some("line1"), Some("line2"), Some("line3"))))
  }
}
