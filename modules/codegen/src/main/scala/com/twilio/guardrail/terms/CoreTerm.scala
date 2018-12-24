package com.twilio.guardrail
package terms

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import com.twilio.guardrail.languages.LA

sealed trait CoreTerm[L <: LA, T]
case class GetDefaultFramework[L <: LA]()                                                              extends CoreTerm[L, String]
case class ExtractGenerator[L <: LA](context: Context)                                                 extends CoreTerm[L, FunctionK[CodegenApplication, Target]]
case class ParseArgs[L <: LA](args: Array[String], defaultFramework: String)                           extends CoreTerm[L, List[Args]]
case class ValidateArgs[L <: LA](parsed: List[Args])                                                   extends CoreTerm[L, NonEmptyList[Args]]
case class ProcessArgSet[L <: LA](targetInterpreter: FunctionK[CodegenApplication, Target], arg: Args) extends CoreTerm[L, ReadSwagger[Target[List[WriteTree]]]]
