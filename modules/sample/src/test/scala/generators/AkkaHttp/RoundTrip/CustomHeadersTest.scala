package generators.AkkaHttp.RoundTrip

import org.scalatest.{ FlatSpec, Matchers }
import tests.customTypes.customHeader.Implicits.Formatter
import tests.customTypes.customHeader.definitions.Bar
import tests.customTypes.customHeader.{Client, Handler, Resource}
import akka.http.scaladsl.server.{ Route }

class CustomHeadersTest extends FlatSpec with Matchers {

  override implicit val patienceConfig = PatienceConfig(10 seconds, 1 second)

  it should "encode custom headers" in {
    Formatter.show(Bar.V1) shouldBe "v1"
    Formatter.show(Bar.ILikeSpaces) shouldBe "i like spaces"
  }

}
