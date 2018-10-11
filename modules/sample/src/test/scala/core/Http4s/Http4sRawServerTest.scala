package core.Http4s

import org.http4s.dsl.io._
import org.http4s.circe._
import org.scalatest.{ FunSuite, Matchers }
import _root_.raw.server.http4s.foo.{ DoFooResponse, FooHandler, FooResource }
import _root_.raw.server.http4s.{ definitions => sdefs }
import cats.effect.IO
import org.http4s.{ EntityEncoder, Response }

class Http4sRawServerTest extends FunSuite with Matchers {
  test("raw server response") {
    implicit val fooEncoder: EntityEncoder[IO, sdefs.Foo] = jsonEncoderOf[IO, sdefs.Foo]
    new FooResource[IO]().routes(new FooHandler[IO] {
      override def doFoo(respond: DoFooResponse.type)(): IO[Response[IO]] = Ok(sdefs.Foo(1234L))
    })
  }
}
