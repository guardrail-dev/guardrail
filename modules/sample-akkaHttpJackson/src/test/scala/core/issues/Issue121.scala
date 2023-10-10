package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.instances.future._
import core.TestImplicits
import issues.issue121.server.akkaHttpJackson.Resource
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.SpanSugar._
import scala.concurrent.Future

class Issue121Suite extends AnyFunSuite with TestImplicits with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(10 seconds, 1 second)

  test("akka-http server can respond with 204") {
    import issues.issue121.server.akkaHttpJackson.Handler
    val route = Resource.routes(new Handler {
      override def deleteFoo(respond: Resource.DeleteFooResponse.type)(id: Long): Future[Resource.DeleteFooResponse] =
        Future.successful(respond.NoContent)
    })

    Delete("/entity").withEntity(FormData("id" -> "1234").toEntity) ~> route ~> check {
      status should equal(StatusCodes.NoContent)
      response.entity.contentType should equal(ContentTypes.NoContentType)
      response.entity.contentLengthOption should equal(Some(0))
    }
  }

  test("akka-http client can respond with 204") {
    import issues.issue121.client.akkaHttpJackson.Client

    def noContentResponse: HttpRequest => Future[HttpResponse] = _ => Future.successful(HttpResponse(204))

    /* Correct mime type
     * Missing content
     */
    Client
      .httpClient(noContentResponse, "http://localhost:80")
      .deleteFoo(1234)
      .fold(
        _ => failTest("Error"),
        _.fold(
          handleNoContent = ()
        )
      )
      .futureValue
  }
}
