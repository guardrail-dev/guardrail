package com.twilio.guardrail.sbt

class ExampleCase(val file: java.io.File, val prefix: String, val cliArgs: List[String]) {
  def args(cliArgs: String*): ExampleCase = new ExampleCase(file, prefix, cliArgs=cliArgs.toList)
}
object ExampleCase {
  def apply(file: java.io.File, prefix: String): ExampleCase = new ExampleCase(file, prefix, List.empty)
  def unapply(value: ExampleCase): Option[(java.io.File, String, List[String])] = Option((value.file, value.prefix, value.cliArgs))
}
