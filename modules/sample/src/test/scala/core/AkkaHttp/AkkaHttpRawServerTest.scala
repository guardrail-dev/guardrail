package core.AkkaHttp

import org.scalatest.{ EitherValues, FunSuite, Matchers }
import org.scalatest.concurrent.ScalaFutures
import _root_.raw.server.akkaHttp.foo.{ FooHandler, FooResource }
import _root_.raw.server.akkaHttp.{ definitions => sdefs }
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import scala.concurrent.Future
import akka.http.scaladsl.marshalling.Marshal

class AkkaHttpRawServerTest extends FunSuite with Matchers with EitherValues with ScalaFutures with ScalatestRouteTest {
  test("raw server response") {
    FooResource.routes(new FooHandler {
      def doFoo(respond: FooResource.doFooResponse.type)(): scala.concurrent.Future[HttpResponse] =
        Marshal(respond.Created(sdefs.Foo(1234L))).to[HttpResponse]
    })
  }
}
