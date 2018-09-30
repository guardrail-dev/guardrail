package core.Http4s

import _root_.tracer.clients.{ http4s => cdefs }
import _root_.tracer.servers.http4s.addresses.{ AddressesHandler, AddressesResource, GetAddressResponse, GetAddressesResponse }
import _root_.tracer.servers.http4s.users.{ GetUserResponse, UsersHandler, UsersResource }
import _root_.tracer.servers.{ http4s => sdefs }
import _root_.tracer.clients.http4s.users.UsersClient
import _root_.tracer.clients.http4s.addresses.AddressesClient
import _root_.tracer.servers.http4s.Http4sImplicits.TraceBuilder
import cats.data.Kleisli
import cats.effect.IO
import org.http4s.{ Header, HttpService, Request }
import org.http4s.client.Client
import org.http4s.syntax.StringSyntax
import org.scalatest.{ EitherValues, FunSuite, Matchers }

class Http4sFullTracerTest extends FunSuite with Matchers with EitherValues with StringSyntax {

  val traceHeaderKey          = "tracer-label"
  def log(line: String): Unit = ()

  def trace: String => Request[IO] => TraceBuilder[IO] = { name => request =>
    // In a real environment, this would be where you could establish a new
    // tracing context and inject that fresh header value.
    log(s"Expecting all requests to have ${traceHeaderKey} header.")
    traceBuilder(request.headers.get(traceHeaderKey.ci).get.value)
  }

  def traceBuilder(parentValue: String): TraceBuilder[IO] = { name => httpClient =>
    httpClient.copy(open = Kleisli { req =>
      // Rudimentary testing. As we have the response object in res, we could
      // also log error codes or other interesting metrics.
      val before = System.currentTimeMillis
      for {
        res <- httpClient.open(req.putHeaders(Header(traceHeaderKey, parentValue)))
        after = System.currentTimeMillis
        ()    = log(s"Request took ${after - before}ms")
      } yield res
    })
  }

  test("full tracer: passing headers through multiple levels") {
    // Establish the "Address" server
    val server2: HttpService[IO] =
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
    val server1: HttpService[IO] =
      new UsersResource(trace).routes(
        new UsersHandler[IO] {
          // ... using the "Address" server explicitly in the addressesClient
          val addressesClient = AddressesClient.httpClient(Client.fromHttpService(server2))
          def getUser(respond: GetUserResponse.type)(id: String)(traceBuilder: TraceBuilder[IO]) =
            addressesClient
              .getAddress(traceBuilder, "addressId")
              .map {
                case cdefs.addresses.GetAddressResponse.Ok(address) => respond.Ok(sdefs.definitions.User("1234", sdefs.definitions.UserAddress(address.line1, address.line2, address.line3)))
                case cdefs.addresses.GetAddressResponse.NotFound    => respond.NotFound
              }
        }
      )

    // Build a UsersClient using the User server
    val usersClient = UsersClient.httpClient(Client.fromHttpService(server1))
    // As this is the entry point, we either have a tracing header from
    // somewhere else, or we generate one for top-level request.
    val testTrace = traceBuilder("top-level-request")

    // Make a request against the mock servers using a hard-coded user ID
    val retrieved: cdefs.users.GetUserResponse = usersClient.getUser(testTrace, "1234").attempt.unsafeRunSync().right.value

    retrieved shouldBe cdefs.users.GetUserResponse.Ok(cdefs.definitions.User("1234", cdefs.definitions.UserAddress(Some("line1"), Some("line2"), Some("line3"))))
  }
}
