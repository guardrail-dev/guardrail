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
    val expected = """
      |   INFO    first second: one
      |   INFO    first second: two
      |   INFO    first second: three
      """.trim.stripMargin

    {
      implicit val logLevel = LogLevels.Debug
      logEntries.show should ===(expected)
    }
  }

  test("Structured Logger handles prependedLogs") {
    val start = (List.empty[Args], List(
      "--defaults",
      "--specPath", "modules/sample/src/main/resources/issues/issue1.yaml",
      "--specPath", "modules/sample/src/main/resources/issues/issue2.yaml",
      "--specPath", "modules/sample/src/main/resources/issues/issue3.yaml",
      ))
    val defaultArgs =
      Args.empty
        .withContext(Args.empty.context)
        .withDefaults(true)
    type From = (List[Args], List[String])
    type To   = List[Args]
    import Target.log.debug
    val structure = {
      Target.loggerEnabled.set(true)  // Before program
      val value = Target.log.function("parseArgs") {
        cats.FlatMap[Target].tailRecM[From, To](start) { case pair @ (sofars, rest) =>
          val empty = sofars
            .filter(_.defaults)
            .lastOption
            .getOrElse(defaultArgs)
            .withDefaults(false)

          def Continue(x: From): Target[Either[From, To]] = Target.pure(Either.left(x))
          def Return(x: To): Target[Either[From, To]]     = Target.pure(Either.right(x))
          def Bail(x: Error): Target[Either[From, To]]    = Target.raiseError(x)
          for {
            _ <- debug(s"Processing: ${rest.take(5).mkString(" ")}${if (rest.length > 3) "..." else ""} of ${rest.length}")
            step <- pair match {
              case (already, Nil) => debug("Finished") >> Return(already)
              case (already, "--defaults" :: xs) => Continue((empty.withDefaults(true) :: already, xs))
              case (Nil, xs @ (_ :: _)) => Continue((empty :: Nil, xs))
              case (sofar :: already, "--specPath" :: value :: xs) => Continue((sofar.withSpecPath(Option(value)) :: already, xs))
              case (_, unknown) =>
                debug(s"Unknown argument: ${unknown}") >> Bail(UnknownArguments(unknown))
            }
          } yield step
        }
      }
      Target.loggerEnabled.set(false)  // After program
      value
    }

    val logEntries = structure.logEntries
    val expected = """
      |  DEBUG    parseArgs: Processing: --defaults --specPath modules/sample/src/main/resources/issues/issue1.yaml --specPath modules/sample/src/main/resources/issues/issue2.yaml... of 7
      |  DEBUG    parseArgs: Processing: --specPath modules/sample/src/main/resources/issues/issue1.yaml --specPath modules/sample/src/main/resources/issues/issue2.yaml --specPath... of 6
      |  DEBUG    parseArgs: Processing: --specPath modules/sample/src/main/resources/issues/issue2.yaml --specPath modules/sample/src/main/resources/issues/issue3.yaml... of 4
      |  DEBUG    parseArgs: Processing: --specPath modules/sample/src/main/resources/issues/issue3.yaml of 2
      |  DEBUG    parseArgs: Processing:  of 0
      |  DEBUG    parseArgs: Finished
      """.trim.stripMargin

    {
      implicit val logLevel = LogLevels.Debug
      logEntries.entries.length should ===(3)  // pushLogger, StructuredLogBlock, pop
      logEntries.show should ===(expected)
    }
  }
}
