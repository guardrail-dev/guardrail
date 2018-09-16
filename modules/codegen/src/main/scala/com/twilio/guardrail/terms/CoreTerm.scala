package com.twilio.guardrail
package terms

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import scala.meta._
import com.twilio.guardrail.generators.GeneratorSettings

sealed trait CoreTerm[T]
case object GetDefaultFramework                                     extends CoreTerm[String]
case class ExtractGenerator(context: Context)                       extends CoreTerm[FunctionK[CodegenApplication, Target]]
case class ExtractGeneratorSettings(context: Context)               extends CoreTerm[GeneratorSettings]
case class ParseArgs(args: Array[String], defaultFramework: String) extends CoreTerm[List[Args]]
case class ValidateArgs(parsed: List[Args])                         extends CoreTerm[NonEmptyList[Args]]
case class ProcessArgSet(targetInterpreter: FunctionK[CodegenApplication, Target], arg: Args)
    extends CoreTerm[ReadSwagger[Target[List[WriteTree]]]]
