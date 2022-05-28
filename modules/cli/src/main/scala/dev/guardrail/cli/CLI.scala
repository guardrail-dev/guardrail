package dev.guardrail.cli

object CLI extends CLICommon {
  def main(args: Array[String]): Unit = {
    val result = processArgs(args)
    sys.exit(result.exitStatus)
  }
}
