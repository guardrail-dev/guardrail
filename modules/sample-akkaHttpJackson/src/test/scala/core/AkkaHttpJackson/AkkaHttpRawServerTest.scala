package core.AkkaHttpJackson

import _root_.raw.server.akkaHttpJackson.foo.{ FooHandler, FooResource }
import _root_.raw.server.akkaHttpJackson.{ definitions => sdefs }
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import core.TestImplicits
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AkkaHttpRawServerTest extends AnyFunSuite with TestImplicits with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  test("raw server response") {
    FooResource.routes(new FooHandler {
      def doFoo(respond: FooResource.DoFooResponse.type)(): scala.concurrent.Future[HttpResponse] =
        Marshal(respond.Created(sdefs.Foo(1234L))).to[HttpResponse]
    })
  }
}
