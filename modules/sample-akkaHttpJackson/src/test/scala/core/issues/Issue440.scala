package core.issues

import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class Issue440Suite extends AnyFunSuite with Matchers {
  test("Construct Issue 440 elements") {
    import issues.issue440.server.akkaHttpJackson.definitions.{ Bar, Enum, Foo }
    Foo(NonEmptyList.of(Bar(1L)), NonEmptyList.of(Bar(2L)), NonEmptyMap.of(("a", Enum.B), ("b", Enum.C)))
  }
}
