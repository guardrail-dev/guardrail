package swagger

import _root_.tracer.clients.{definitions => cdefs}
import _root_.tracer.servers.addresses.{AddressesHandler, AddressesResource}
import _root_.tracer.servers.users.{UsersHandler, UsersResource}
import _root_.tracer.servers.{definitions => sdefs}
import _root_.tracer.clients.users.UsersClient
import _root_.tracer.clients.addresses.AddressesClient
import _root_.tracer.servers.AkkaHttpImplicits.TraceBuilder
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.instances.future._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{EitherValues, FunSuite, Matchers}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

class FullTracer extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest with IntegrationPatience {

  val traceHeaderKey = "tracer-label"
  def log(line: String): Unit = ()

  def trace(implicit ec: ExecutionContext): String => Directive1[TraceBuilder] = {
    name =>
      // In a real environment, this would be where you could establish a new
      // tracing context and inject that fresh header value.
      log(s"Expecting all requests to have ${traceHeaderKey} header, otherwise 400.")
      for {
        traceValue <- headerValueByName(traceHeaderKey)
      } yield {
        traceBuilder(traceValue)
      }
  }

  def traceBuilder(parentValue: String)(implicit ec: ExecutionContext): TraceBuilder = {
    name => httpClient =>
    { req =>
      // Rudimentary testing. As we have the response object in res, we could
      // also log error codes or other interesting metrics.
      val before = System.currentTimeMillis
      for {
        res <- httpClient(req.mapHeaders(RawHeader(traceHeaderKey, parentValue) +: _))
        after = System.currentTimeMillis
        () = log(s"Request took ${after - before}ms")
      } yield res
    }
  }

  test("full tracer: passing headers through multiple levels") {
    // Establish the "Address" server
    val server2: HttpRequest => Future[HttpResponse] = Route.asyncHandler(AddressesResource.routes(new AddressesHandler {
      def getAddress(respond: AddressesResource.getAddressResponse.type)(id: String)(traceBuilder: TraceBuilder) = {
        Future.successful(if (id == "addressId") {
          respond.OK(sdefs.Address(Some("line1"), Some("line2"), Some("line3")))
        } else respond.NotFound)
      }
      def getAddresses(respond: AddressesResource.getAddressesResponse.type)()(traceBuilder: TraceBuilder) =
        Future.successful(respond.NotFound)
    }, trace))

    // Establish the "User" server
    val server1: HttpRequest => Future[HttpResponse] = Route.asyncHandler(UsersResource.routes(new UsersHandler {
      // ... using the "Address" server explicitly in the addressesClient
      val addressesClient = AddressesClient.httpClient(server2)
      def getUser(respond: UsersResource.getUserResponse.type)(id: String)(traceBuilder: TraceBuilder) = {
        addressesClient.getAddress(traceBuilder, "addressId")
          .fold(_ => respond.NotFound, { address =>
            respond.OK(sdefs.User("1234", sdefs.UserAddress(address.line1, address.line2, address.line3)))
          })
      }
    }, trace))

    // Build a UsersClient using the User server
    val usersClient = UsersClient.httpClient(server1)
    // As this is the entry point, we either have a tracing header from
    // somewhere else, or we generate one for top-level request.
    val testTrace = traceBuilder("top-level-request")

    // Make a request against the mock servers using a hard-coded user ID
    val retrieved: cdefs.User = usersClient.getUser(testTrace, "1234").value.futureValue.right.value
    
    retrieved shouldBe(cdefs.User("1234", cdefs.UserAddress(Some("line1"), Some("line2"), Some("line3"))))
  }
}
