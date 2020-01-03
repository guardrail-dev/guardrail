package core

import cats.free.{ Free, GuardrailFreeHacks }
import cats.implicits._
import com.twilio.guardrail._
import com.twilio.swagger.core._
import com.twilio.guardrail.terms.{ SwaggerTerm, SwaggerTerms }

import org.scalatest.{ FunSuite, Matchers }
import cats.arrow.FunctionK
import com.twilio.guardrail.languages.ScalaLanguage
import cats.data.EitherK
import com.twilio.guardrail.generators.SwaggerGenerator

sealed trait Algebra[A]
case class GenName(value: String)     extends Algebra[String]
case class ToUpper(value: String)     extends Algebra[String]
case class ToLower(value: String)     extends Algebra[String]
case class Passthrough(value: String) extends Algebra[String]

/** GuardrailFreeHacksSuite
  *
  *  For documentation, please see GuardrailFreeHacks itself.
  */
class GuardrailFreeHacksSuite extends FunSuite with Matchers {
  /*
  def genLogEntries(): StructuredLogger = {
    type Program[A] = EitherK[Algebra, SwaggerTerm[ScalaLanguage, ?], A]
    val Sw = SwaggerTerms.swaggerTerm[ScalaLanguage, Program]

    def passthrough(value: String): Free[Program, String] =
      Free.inject[Algebra, Program](Passthrough(value + "."))

    def genName(value: String): Free[Program, String] =
      Sw.log.function("genName")(
        Free.inject[Algebra, Program](GenName("name 1")) <* Sw.log.info("done")
      )

    def toUpper(value: String): Free[Program, String] =
      Sw.log.function("toUpper")(
        for {
          a <- Free.inject[Algebra, Program](ToUpper(value))
          _ <- Sw.log.debug("foo")
          b <- passthrough(a)
          c <- passthrough(b)
          d <- passthrough(c)
          _ <- Sw.log.warning("toUpper may occasionally fail!")
        } yield d
      )

    def toLower(value: String): Free[Program, String] =
      Sw.log.function("toLower")(
        Free.inject[Algebra, Program](ToLower(value)) <* Sw.log.debug("bar")
      )

    def doWork(v1: String, v2: String): Free[Program, String] =
      Sw.log.function("doWork") {
        for {
          _ <- Sw.log.info("work starting")
          a <- toUpper(v1)
          b <- toLower(v2)
          _ <- Sw.log.info("work finished")
        } yield a + b
      }

    val program = for {
      n1 <- genName("name 1")
      n2 <- genName("name 2")
      r1 <- doWork(n1, n2)
      r2 <- doWork(n2, n1)
    } yield r1 == r2

    val interp = new FunctionK[Algebra, Target] {
      def apply[A](term: Algebra[A]): Target[A] = term match {
        case GenName(a)     => Target.pure(a)
        case ToUpper(a)     => Target.pure(a.toUpperCase)
        case ToLower(a)     => Target.pure(a.toLowerCase)
        case Passthrough(a) => Target.log.error("Alert!") *> Target.pure(a)
      }
    }
    val ignoredTerms = Set("LogPush", "LogPop", "LogDebug", "LogInfo", "LogWarning", "LogError")
    GuardrailFreeHacks
      .injectLogs(program, ignoredTerms, Sw.log.push, Sw.log.pop, Free.pure(()))
      .foldMap(interp or SwaggerGenerator.apply[ScalaLanguage])
      .value
      .runEmptyS
  }

  test("Nested function calls work (DEBUG)") {
    val expected = """
      |   INFO    genName: done
      |   INFO    genName: done
      |   INFO    doWork: work starting
      |  DEBUG    doWork toUpper: foo
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      |WARNING    doWork toUpper: toUpper may occasionally fail!
      |  DEBUG    doWork toLower: bar
      |   INFO    doWork: work finished
      |   INFO    doWork: work starting
      |  DEBUG    doWork toUpper: foo
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      |WARNING    doWork toUpper: toUpper may occasionally fail!
      |  DEBUG    doWork toLower: bar
      |   INFO    doWork: work finished
      """.trim.stripMargin

    implicit val logLevel = LogLevels.Debug
    genLogEntries().show should ===(expected)
  }

  test("Nested function calls work (INFO)") {
    val expected = """
      |   INFO    genName: done
      |   INFO    genName: done
      |   INFO    doWork: work starting
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      |WARNING    doWork toUpper: toUpper may occasionally fail!
      |   INFO    doWork: work finished
      |   INFO    doWork: work starting
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      |WARNING    doWork toUpper: toUpper may occasionally fail!
      |   INFO    doWork: work finished
      """.trim.stripMargin

    implicit val logLevel = LogLevels.Info
    genLogEntries().show should ===(expected)
  }

  test("Nested function calls work (WARNING)") {
    val expected = """
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      |WARNING    doWork toUpper: toUpper may occasionally fail!
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      |WARNING    doWork toUpper: toUpper may occasionally fail!
      """.trim.stripMargin

    implicit val logLevel = LogLevels.Warning
    genLogEntries().show should ===(expected)
  }

  test("Nested function calls work (ERROR)") {
    val expected = """
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1.): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1..): Alert!
      |  ERROR    doWork toUpper Passthrough(NAME 1...): Alert!
      """.trim.stripMargin

    implicit val logLevel = LogLevels.Error
    genLogEntries().show should ===(expected)
  }

  test("Nested function calls work (SILENT)") {
    val expected = """
      """.trim.stripMargin

    implicit val logLevel = LogLevels.Silent
    genLogEntries().show should ===(expected)
  }
 */
}
