package core.issues

import akka.http.scaladsl.model.{ FormData, _ }
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.{ RejectionHandler, Route }
import akka.http.scaladsl.testkit.ScalatestRouteTest
import core.TestImplicits
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar._
import scala.concurrent.Future

/** Changes
  *
  *   - Required String form param should accept empty string
  *   - Option String for param should accept emtpy string
  */
class Issue405 extends AnyFunSuite with TestImplicits with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  implicit val rejectionHandler: RejectionHandler = RejectionHandler
    .newBuilder()
    .handle { case rej =>
      complete(HttpResponse(StatusCodes.BadRequest, entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, s"Request failed with $rej")))
    }
    .result()

  test("Empty string for a required form param should parse as empty string") {
    import issues.issue405.server.akkaHttpJackson.{ Handler, Resource }

    val route = Route.seal(Resource.routes(new Handler {
      override def foo(respond: Resource.FooResponse.type)(bar: String, baz: Option[String]): Future[Resource.FooResponse] =
        Future.successful(respond.OK(s"Bar is '$bar'"))
    }))

    /* Pass empty string to required Bar param */
    Post("/v1/Foo", FormData(Map("Bar" -> ""))) ~> route ~> check {
      response.status shouldBe (StatusCodes.OK)
      responseAs[String] shouldBe "Bar is ''"
    }
  }

  test("Empty string for an optional form param should parse as empty string") {
    import issues.issue405.server.akkaHttpJackson.{ Handler, Resource }

    val route = Route.seal(Resource.routes(new Handler {
      override def foo(respond: Resource.FooResponse.type)(bar: String, baz: Option[String]): Future[Resource.FooResponse] = {
        val msg = baz.map(s => s"present: '$s'").getOrElse("missing")
        Future.successful(respond.OK(s"Baz is $msg"))
      }
    }))

    /* Pass empty string to required Bar param */
    Post("/v1/Foo", FormData(Map("Bar" -> "whatevs", "Baz" -> ""))) ~> route ~> check {
      response.status shouldBe (StatusCodes.OK)
      responseAs[String] shouldBe "Baz is present: ''"
    }
  }

  test("Omitting a required parameter should still reject the request") {
    import issues.issue405.server.akkaHttpJackson.{ Handler, Resource }

    val route = Route.seal(Resource.routes(new Handler {
      override def foo(respond: Resource.FooResponse.type)(bar: String, baz: Option[String]): Future[Resource.FooResponse] =
        Future.successful(respond.OK(s"Bar is '$bar'"))
    }))

    /* Pass empty string to required Bar param */
    Post("/v1/Foo", FormData()) ~> route ~> check {
      response.status shouldBe (StatusCodes.BadRequest)
      responseAs[String] shouldBe "Request failed with MissingFormFieldRejection(Bar)"
    }
  }

  test("Omitting an optional parameter should still pass None to the endpoint") {
    import issues.issue405.server.akkaHttpJackson.{ Handler, Resource }

    val route = Route.seal(Resource.routes(new Handler {
      override def foo(respond: Resource.FooResponse.type)(bar: String, baz: Option[String]): Future[Resource.FooResponse] = {
        val msg = baz.map(s => s"present: '$s'").getOrElse("missing")
        Future.successful(respond.OK(s"Baz is $msg"))
      }
    }))

    /* Pass empty string to required Bar param */
    Post("/v1/Foo", FormData(Map("Bar" -> "yo"))) ~> route ~> check {
      response.status shouldBe (StatusCodes.OK)
      responseAs[String] shouldBe "Baz is missing"
    }
  }
}
