package com.twilio.guardrail

sealed trait Error
case class MissingArg(arg: Args, name: Error.ArgName)         extends Error
case class UnknownArguments(args: List[String])               extends Error
case class UnknownFramework(name: String)                     extends Error
case class UnparseableArgument(name: String, message: String) extends Error
case object NoArgsSpecified                                   extends Error
case object NoFramework                                       extends Error
case object PrintHelp                                         extends Error
object Error {
  case class ArgName(value: String) extends AnyVal
}
