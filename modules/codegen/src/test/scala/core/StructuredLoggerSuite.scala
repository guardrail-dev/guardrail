package core
import dev.guardrail._
import dev.guardrail.core._

import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class StructuredLoggerSuite extends AnyFunSuite with Matchers {
  test("Structured Logger can nest functions") {
    Target.loggerEnabled.set(true)
    val structure =
      Target.log.function("first") {
        Target.log.function("second") {
          for {
            _ <- Target.log.info("one")
            _ <- Target.log.info("two")
            _ <- Target.log.info("three")
          } yield ()
        }
      }
    Target.loggerEnabled.set(false)
    val logEntries = structure.logEntries
    val expected   = """
      |   INFO    first second: one
      |   INFO    first second: two
      |   INFO    first second: three
      """.trim.stripMargin

    {
      implicit val logLevel = LogLevels.Debug
      logEntries.show should ===(expected)
    }
  }
}
