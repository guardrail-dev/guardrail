package com.twilio.guardrail.sbt

class ExampleCase(val file: java.io.File, val prefix: String, val cliArgs: List[String],val frameworks: Option[Set[String]]) {
  def args(cliArgs: String*): ExampleCase = new ExampleCase(file, prefix, cliArgs=cliArgs.toList, frameworks = frameworks)
  def frameworks(frameworks: Set[String]): ExampleCase = new ExampleCase(file,prefix,cliArgs,Some(frameworks))
}
object ExampleCase {
  def apply(file: java.io.File, prefix: String): ExampleCase = new ExampleCase(file, prefix, List.empty, None)
  def unapply(value: ExampleCase): Option[(java.io.File, String, List[String],Option[Set[String]])] = Option((value.file, value.prefix, value.cliArgs, value.frameworks))
}
