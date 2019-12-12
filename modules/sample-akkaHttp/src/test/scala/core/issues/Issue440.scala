package core.issues

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshaller
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.implicits._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.SpanSugar._
import org.scalatest.{ EitherValues, FunSuite, Matchers }
import scala.concurrent.Future
import io.circe._

class Issue440Suite extends FunSuite with Matchers {
  test("Construct Issue 440 elements") {
    import issues.issue440.server.akkaHttp.definitions.{ Bar, Enum, Foo }
    Foo(NonEmptyList.of(Bar(1L)), NonEmptyList.of(Bar(2L)), NonEmptyMap.of(("a", Enum.B), ("b", Enum.C)))
  }
}
