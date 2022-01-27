package dev.guardrail.cli

import dev.guardrail.generators.GeneratorMappings

object CLI extends CLICommon {
  def languages = GeneratorMappings.defaultLanguages
  def main(args: Array[String]): Unit = {
    val result = processArgs(args)
    sys.exit(result.exitStatus)
  }
}
