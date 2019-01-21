package swagger

import org.scalatest.{ FunSuite, Matchers }

class EscapingTest extends FunSuite with Matchers {
  test("akka-http: Properly escape parameters") {
    import examples.client.akkaHttp.Implicits
    import examples.client.akkaHttp.AkkaHttpImplicits._
    Implicits.Formatter.addPath("foo bar baz") shouldEqual "foo%20bar%20baz"
    Implicits.Formatter.addPath("foo/bar") shouldEqual "foo%2Fbar"
    Implicits.Formatter.addArg("my key", "3=foo/bar baz!") shouldEqual "&my+key=3%3Dfoo/bar+baz!"
  }

  test("http4s: Properly escape parameters") {
    import examples.client.http4s.Implicits
    import examples.client.http4s.Http4sImplicits._
    Implicits.Formatter.addPath("foo bar baz") shouldEqual "foo%20bar%20baz"
    Implicits.Formatter.addPath("foo/bar") shouldEqual "foo%2Fbar"
    Implicits.Formatter.addArg("my key", "3=foo/bar baz!") shouldEqual "&my%20key=3%3Dfoo/bar%20baz%21"
  }
}
