package swagger

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import examples.client.endpoints.Implicits
import examples.client.endpoints.EndpointsImplicits._
import endpoints.{ algebra, xhr }

class EscapingTest extends AnyFunSuite with Matchers {
  object Instances
      extends algebra.circe.JsonEntitiesFromCodec
      with xhr.JsonEntitiesFromCodec
      with xhr.faithful.Endpoints
      with XhrAddPathSegments
      with XhrFormData
  import Instances._
  test("Properly escape parameters") {
    Implicits.Formatter.addPath("foo bar baz") shouldEqual "foo%20bar%20baz"
    Implicits.Formatter.addPath("foo/bar") shouldEqual "foo%2Fbar"
    Implicits.Formatter.addArg("my key", "3=foo/bar baz!") shouldEqual "&my%20key=3%3Dfoo%2Fbar%20baz!"
  }
}
