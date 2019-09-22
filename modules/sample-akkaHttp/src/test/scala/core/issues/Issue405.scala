package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.RejectionHandler
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{EitherValues, FunSuite, Matchers}
import akka.http.scaladsl.server.Directives.complete

import scala.concurrent.Future

/** Changes
  *
  * - Required String form param should accept empty string
  * - Option String for param should accept emtpy string
  */
class Issue405 extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  test("Empty string for a required form param should parse as empty string") {
    import issues.issue405.server.akkaHttp.{ Handler, Resource }
    import issues.issue405.server.akkaHttp.definitions._

    implicit def rejectionHandler =
      RejectionHandler.newBuilder()
        .handle { case rej =>
          complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, s"Request failed with $rej")))
        }
        .result()

    val route = Resource.routes(new Handler {
      override def foo(respond: Resource.fooResponse.type)(bar: String, baz: Option[String]): Future[Resource.fooResponse] =
        Future.successful(respond.NoContent)
    })

    /* Pass empty string to required Bar param */
    Post("/Foo")
      .withEntity(
        Multipart
          .FormData(
            Multipart.FormData.BodyPart.Strict("Bar", "")
          )
          .toEntity
      ) ~> route ~> check {
      response.status shouldBe (StatusCodes.NoContent)
    }
  }

  test("Empty string for an optional form param should parse as empty string") {
    import issues.issue405.server.akkaHttp.{ Handler, Resource }
    import issues.issue405.server.akkaHttp.definitions._

    val route = Resource.routes(new Handler {
      override def foo(respond: Resource.fooResponse.type)(bar: String, baz: Option[String]): Future[Resource.fooResponse] =
        baz
          .map(_ => Future.successful(respond.NoContent))
          .getOrElse(Future.failed(new Exception("Baz was parsed as None")))
    })

    /* Pass empty string to required Bar param */
    Post("/Foo")
      .withEntity(
        Multipart
          .FormData(
            Multipart.FormData.BodyPart.Strict("Baz", "")
          )
          .toEntity
      ) ~> route ~> check {
      response.status shouldBe (StatusCodes.NoContent)
    }
  }
}
