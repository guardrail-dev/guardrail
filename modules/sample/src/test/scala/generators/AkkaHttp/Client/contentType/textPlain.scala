package tests.generators.AkkaHttp.Client.contentType

import _root_.tests.contentTypes.textPlain.Implicits.IgnoredEntity
import _root_.tests.contentTypes.textPlain.foo.FooClient
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

class TextPlainTest extends FunSuite with Matchers with EitherValues with EitherTValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(1000.millis, 1000.millis)
  test("Plain text should be emitted for optional parameters") {
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
    new EitherTValuable(fooClient.doFoo(Some("sample"))).rightValue.futureValue shouldBe IgnoredEntity.empty
  }

  test("Plain text should be emitted for required parameters") {
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
    new EitherTValuable(fooClient.doBar("sample")).rightValue.futureValue shouldBe IgnoredEntity.empty
  }
}
