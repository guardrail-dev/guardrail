package core.issues

import akka.http.scaladsl.model.{ ContentTypes, StatusCodes }
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.concurrent.Future
import akka.http.scaladsl.testkit.ScalatestRouteTest

class Issue572 extends AnyFunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  test("akka-http server can propagate headers to responses") {
    import issues.issue572.server.akkaHttp.{ Handler, Resource }
    import issues.issue572.server.akkaHttp.definitions.{ Foo }
    val route = Resource.routes(new Handler {
      override def getFoo(respond: Resource.GetFooResponse.type)(): scala.concurrent.Future[Resource.GetFooResponse] =
        Future.successful(respond.OK(Foo(Some("success")), "requiredHeader", Some("optionalHeader")))
      override def getBar(respond: Resource.GetBarResponse.type)(): scala.concurrent.Future[Resource.GetBarResponse] =
        Future.successful(respond.NoContent("requiredHeader", Some("optionalHeader")))
    })

    Get("/foo") ~> route ~> check {
      status should equal(StatusCodes.OK)
      response.entity.contentType should equal(ContentTypes.`application/json`)
      response.headers should have size 2
      response.headers.map(_.name) should contain theSameElementsAs Seq("X-Required-Header", "X-Optional-Header")
      response.headers.map(_.value) should contain theSameElementsAs Seq("requiredHeader", "optionalHeader")
    }

    Get("/bar") ~> route ~> check {
      status should equal(StatusCodes.NoContent)
      response.entity.contentType should equal(ContentTypes.NoContentType)
      response.headers should have size 2
      response.headers.map(_.name) should contain theSameElementsAs Seq("X-Required-Header", "X-Optional-Header")
      response.headers.map(_.value) should contain theSameElementsAs Seq("requiredHeader", "optionalHeader")
    }
  }

  test("akka-http server correctly propagates only required header") {
    import issues.issue572.server.akkaHttp.{ Handler, Resource }
    import issues.issue572.server.akkaHttp.definitions.{ Foo }
    val route = Resource.routes(new Handler {
      override def getFoo(respond: Resource.GetFooResponse.type)(): scala.concurrent.Future[Resource.GetFooResponse] =
        Future.successful(respond.OK(Foo(Some("success")), "requiredHeader", None))
      override def getBar(respond: Resource.GetBarResponse.type)(): scala.concurrent.Future[Resource.GetBarResponse] =
        Future.successful(respond.NoContent("requiredHeader", None))
    })

    Get("/foo") ~> route ~> check {
      status should equal(StatusCodes.OK)
      response.entity.contentType should equal(ContentTypes.`application/json`)
      response.headers should have size 1
      response.headers.map(_.name) should contain theSameElementsAs Seq("X-Required-Header")
      response.headers.map(_.value) should contain theSameElementsAs Seq("requiredHeader")
    }

    Get("/bar") ~> route ~> check {
      status should equal(StatusCodes.NoContent)
      response.entity.contentType should equal(ContentTypes.NoContentType)
      response.headers should have size 1
      response.headers.map(_.name) should contain theSameElementsAs Seq("X-Required-Header")
      response.headers.map(_.value) should contain theSameElementsAs Seq("requiredHeader")
    }
  }
}
