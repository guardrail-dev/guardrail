package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshaller
import cats.instances.future._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{ EitherValues, FunSuite, Matchers }
import scala.concurrent.Future
import io.circe._
import _root_.jawn.IncompleteParseException

/** Changes
  *
  * - Server request body validation
  * - Client responses
  *   - No content vs Partial content vs Invalid content
  * - Polymorphic discriminator error messages
  */
class Issue184Suite extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  test("akka-http server request body validation") {
    import issues.issue184.server.akkaHttp.{ Handler, Resource }
    import issues.issue184.server.akkaHttp.definitions._
    val route = Resource.routes(new Handler {
      override def deleteFoo(respond: Resource.deleteFooResponse.type)(path: String, query: String, form: String): Future[Resource.deleteFooResponse] = {
        Future.successful(respond.NoContent)
      }
    })

    /* Correct mime type
     * Missing header
     */
    Delete("/1234?query=2345")
      .withEntity(
        Multipart
          .FormData(
            Multipart.FormData.BodyPart.Strict("form", "3456")
          )
          .toEntity
      ) ~> route ~> check {
      response.status shouldBe(StatusCodes.NoContent)
    }
  }
}
