package generators.AkkaHttp.Client.contentType

import org.scalatest.{FlatSpec, Matchers}
import tests.customTypes.customHeader.Implicits.Formatter
import tests.customTypes.customHeader.definitions.Bar

class CustomHeadersTest extends FlatSpec with Matchers{

  it should "encode custom headers" in {
    Formatter.show(Bar.V1) shouldBe "v1"
    Formatter.show(Bar.ILikeSpaces) shouldBe "i like spaces"
  }

}
