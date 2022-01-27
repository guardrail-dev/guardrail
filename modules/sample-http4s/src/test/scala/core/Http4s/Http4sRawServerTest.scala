package core.Http4s

import org.http4s.dsl.io._
import org.http4s.circe._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import _root_.raw.server.http4s.foo.{ FooHandler, FooResource }
import _root_.raw.server.http4s.foo.FooResource.DoFooResponse
import _root_.raw.server.http4s.{ definitions => sdefs }
import cats.effect.IO
import org.http4s.{ EntityEncoder, Response }

class Http4sRawServerTest extends AnyFunSuite with Matchers {
  test("raw server response") {
    implicit val fooEncoder: EntityEncoder[IO, sdefs.Foo] = jsonEncoderOf[IO, sdefs.Foo]
    new FooResource[IO]().routes(new FooHandler[IO] {
      override def doFoo(respond: DoFooResponse.type)(): IO[Response[IO]] = Ok(sdefs.Foo(1234L))
    })
  }
}
