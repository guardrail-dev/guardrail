package core

import cats.implicits._
import com.twilio.guardrail._
import com.twilio.swagger.core._

import org.scalatest.{ FunSuite, Matchers }

class StructuredLoggerSuite extends FunSuite with Matchers {
  test("Structured Logger can nest functions") {
    val structure =
      Target.log.function("first") {
        Target.log.function("second") {
          for {
            _ <- Target.log.info("one").apply
            _ <- Target.log.info("two").apply
            _ <- Target.log.info("three").apply
          } yield ()
        }
      }
    val logEntries = structure.value.runEmptyS
    val expected   = """
      |   INFO    first
      |   INFO          second: one
      |   INFO          second: two
      |   INFO          second: three
      """.trim.stripMargin

    {
      implicit val logLevel = LogLevels.Debug
      logEntries.show should ===(expected)
    }
  }
}
