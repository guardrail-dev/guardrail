package com.twilio.guardrail
package terms

import cats.~>
import cats.data.NonEmptyList
import com.twilio.guardrail.languages.LA

sealed trait CoreTerm[L <: LA, T]
case class GetDefaultFramework[L <: LA]()                                                           extends CoreTerm[L, Option[String]]
case class ExtractGenerator[L <: LA](context: Context, defaultFramework: Option[String])            extends CoreTerm[L, CodegenApplication[L, ?] ~> Target]
case class ParseArgs[L <: LA](args: Array[String])                                                  extends CoreTerm[L, List[Args]]
case class ValidateArgs[L <: LA](parsed: List[Args])                                                extends CoreTerm[L, NonEmptyList[Args]]
case class ProcessArgSet[L <: LA](targetInterpreter: CodegenApplication[L, ?] ~> Target, arg: Args) extends CoreTerm[L, ReadSwagger[Target[List[WriteTree]]]]
