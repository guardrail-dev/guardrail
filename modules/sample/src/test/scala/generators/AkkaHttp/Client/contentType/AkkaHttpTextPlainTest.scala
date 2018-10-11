package generators.AkkaHttp.Client.contentType

import _root_.tests.contentTypes.textPlain.client.akkaHttp.AkkaHttpImplicits.IgnoredEntity
import _root_.tests.contentTypes.textPlain.client.akkaHttp.foo.FooClient
import _root_.tests.contentTypes.textPlain.server.akkaHttp.foo.{ FooHandler, FooResource }
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ EitherValues, FunSuite, Matchers }
import scala.concurrent.Future
import scala.concurrent.duration._

import _root_.tests.scalatest.EitherTValues

class AkkaHttpTextPlainTest extends FunSuite with Matchers with EitherValues with EitherTValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(1000.millis, 1000.millis)
  test("Plain text should be emitted for required parameters (raw)") {
    val route: Route = (path("foo") & extractRequestEntity & entity(as[String])) { (entity, value) =>
      complete({
        if (entity.contentType == ContentTypes.`text/plain(UTF-8)` && value == "sample") {
          StatusCodes.Created
        } else {
          StatusCodes.NotAcceptable
        }
      })
    }
    val client: HttpRequest => Future[HttpResponse] = Route.asyncHandler(route)
    val fooClient                                   = FooClient.httpClient(client)
    new EitherTValuable(fooClient.doFoo("sample")).rightValue.futureValue shouldBe IgnoredEntity.empty
  }

  test("Plain text should be emitted for optional parameters (raw)") {
    val route: Route = (path("bar") & extractRequestEntity & entity(as[String])) { (entity, value) =>
      complete({
        if (entity.contentType == ContentTypes.`text/plain(UTF-8)` && value == "sample") {
          StatusCodes.Created
        } else {
          StatusCodes.NotAcceptable
        }
      })
    }
    val client: HttpRequest => Future[HttpResponse] = Route.asyncHandler(route)
    val fooClient                                   = FooClient.httpClient(client)
    new EitherTValuable(fooClient.doBar(Some("sample"))).rightValue.futureValue shouldBe IgnoredEntity.empty
  }

  test("Plain text should be emitted for required parameters") {
    val route: Route = FooResource.routes(new FooHandler {
      def doFoo(
          respond: tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doFooResponse.type
      )(body: String): scala.concurrent.Future[tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doFooResponse] =
        if (body == "sample") {
          Future.successful(respond.Created)
        } else {
          Future.successful(respond.NotAcceptable)
        }
      def doBar(respond: tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doBarResponse.type)(
          body: Option[String]
      ): scala.concurrent.Future[tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doBarResponse] = ???
    })

    val client: HttpRequest => Future[HttpResponse] = Route.asyncHandler(route)
    val fooClient                                   = FooClient.httpClient(client)
    new EitherTValuable(fooClient.doFoo("sample")).rightValue.futureValue shouldBe IgnoredEntity.empty
  }

  test("Plain text should be emitted for present optional parameters") {
    val route: Route = FooResource.routes(new FooHandler {
      def doFoo(
          respond: tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doFooResponse.type
      )(body: String): scala.concurrent.Future[tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doFooResponse] = ???
      def doBar(respond: tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doBarResponse.type)(
          body: Option[String]
      ): scala.concurrent.Future[tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doBarResponse] =
        if (body.contains("sample")) {
          Future.successful(respond.Created)
        } else {
          Future.successful(respond.NotAcceptable)
        }
    })

    val client: HttpRequest => Future[HttpResponse] = Route.asyncHandler(route)
    val fooClient                                   = FooClient.httpClient(client)
    new EitherTValuable(fooClient.doBar(Some("sample"))).rightValue.futureValue shouldBe IgnoredEntity.empty
  }

  test("Plain text should be emitted for missing optional parameters") {
    val route: Route = FooResource.routes(new FooHandler {
      def doFoo(
          respond: tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doFooResponse.type
      )(body: String): scala.concurrent.Future[tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doFooResponse] = ???
      def doBar(respond: tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doBarResponse.type)(
          body: Option[String]
      ): scala.concurrent.Future[tests.contentTypes.textPlain.server.akkaHttp.foo.FooResource.doBarResponse] =
        if (body.isEmpty) {
          Future.successful(respond.Created)
        } else {
          Future.successful(respond.NotAcceptable)
        }
    })

    val client: HttpRequest => Future[HttpResponse] = Route.asyncHandler(route)
    val fooClient                                   = FooClient.httpClient(client)
    new EitherTValuable(fooClient.doBar(None)).rightValue.futureValue shouldBe IgnoredEntity.empty
  }
}
