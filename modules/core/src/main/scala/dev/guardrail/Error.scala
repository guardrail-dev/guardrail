package dev.guardrail

import cats.data.NonEmptyList

sealed trait Error
case class MissingArg(arg: Args, name: Error.ArgName)            extends Error
case class UnknownArguments(args: List[String])                  extends Error
case class UnknownFramework(name: String)                        extends Error
case class MissingDependency(name: String)                       extends Error
case class UnspecifiedModules(choices: Map[String, Set[String]]) extends Error
case class UnparseableArgument(name: String, message: String)    extends Error
case class UnusedModules(found: NonEmptyList[String])            extends Error
case object NoArgsSpecified                                      extends Error
case object NoFramework                                          extends Error
case object PrintHelp                                            extends Error
case class RuntimeFailure(message: String)                       extends Error
case class UserError(message: String)                            extends Error
case class MissingModule(section: String, choices: List[String]) extends Error
case class ModuleConflict(section: String)                       extends Error
object Error {
  case class ArgName(value: String) extends AnyVal
}
