package swagger

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import examples.client.http4s.Implicits
import examples.client.http4s.Http4sImplicits._

class EscapingTest extends AnyFunSuite with Matchers {
  test("Properly escape parameters") {
    Implicits.Formatter.addPath("foo bar baz") shouldEqual "foo%20bar%20baz"
    Implicits.Formatter.addPath("foo/bar") shouldEqual "foo%2Fbar"
    Implicits.Formatter.addArg("my key", "3=foo/bar baz!") shouldEqual "&my%20key=3%3Dfoo/bar%20baz%21"
  }
}
