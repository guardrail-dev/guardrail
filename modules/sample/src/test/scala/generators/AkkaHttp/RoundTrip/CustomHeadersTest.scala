package generators.AkkaHttp.RoundTrip

import akka.actor.ActorSystem
import akka.http.scaladsl.server.{ Route }
import akka.stream.ActorMaterializer
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{ EitherValues, FlatSpec, Matchers }
import scala.concurrent.ExecutionContext.Implicits.global
import tests.customTypes.customHeader.Implicits.Formatter
import tests.customTypes.customHeader.definitions.Bar
import tests.customTypes.customHeader.{ Client, Handler, Resource }
import scala.concurrent.Future

class CustomHeadersTest extends FlatSpec with Matchers with ScalaFutures with EitherValues {

  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  it should "encode custom headers" in {
    Formatter.show(Bar.V1) shouldBe "v1"
    Formatter.show(Bar.ILikeSpaces) shouldBe "i like spaces"
  }

  it should "round-trip encoded values" in {
    implicit val as  = ActorSystem()
    implicit val mat = ActorMaterializer()
    val client = Client.httpClient(Route.asyncHandler(Resource.routes(new Handler {
      def getFoo(respond: Resource.getFooResponse.type)(header: String,
                                                        longHeader: Long,
                                                        customHeader: Bar,
                                                        customOptionHeader: Option[Bar],
                                                        missingCustomOptionHeader: Option[Bar]): Future[Resource.getFooResponse] =
        (header, longHeader, customHeader, customOptionHeader, missingCustomOptionHeader) match {
          case ("foo", 5L, Bar.V1, Some(Bar.V2), None) => Future.successful(respond.OK)
          case _                                       => Future.successful(respond.BadRequest)
        }
    })))

    client.getFoo("foo", 5L, Bar.V1, Some(Bar.V2), None).value.futureValue.right.value
  }

}
