package dev.guardrail.cli

object CLI extends CLICommon {
  val AnsiColor = scala.io.AnsiColor

  def putErrLn(string: String): Unit = System.err.println(string)

  def main(args: Array[String]): Unit = {
    val result = processArgs(args)
    sys.exit(result.exitStatus)
  }
}
