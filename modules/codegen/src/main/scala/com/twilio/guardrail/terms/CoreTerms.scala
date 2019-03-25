package com.twilio.guardrail
package terms

import cats.~>
import cats.InjectK
import cats.data.NonEmptyList
import cats.free.Free
import com.twilio.guardrail.languages.LA

class CoreTerms[L <: LA, F[_]](implicit I: InjectK[CoreTerm[L, ?], F]) {
  def getDefaultFramework: Free[F, Option[String]] =
    Free.inject[CoreTerm[L, ?], F](GetDefaultFramework())
  def extractGenerator(context: Context, defaultFramework: Option[String]): Free[F, CodegenApplication[L, ?] ~> Target] =
    Free.inject[CoreTerm[L, ?], F](ExtractGenerator(context, defaultFramework))
  def parseArgs(args: Array[String]): Free[F, List[Args]] =
    Free.inject[CoreTerm[L, ?], F](ParseArgs(args))
  def validateArgs(parsed: List[Args]): Free[F, NonEmptyList[Args]] =
    Free.inject[CoreTerm[L, ?], F](ValidateArgs(parsed))
  def processArgSet(targetInterpreter: CodegenApplication[L, ?] ~> Target)(args: Args): Free[F, ReadSwagger[Target[List[WriteTree]]]] =
    Free.inject[CoreTerm[L, ?], F](ProcessArgSet(targetInterpreter, args))
}
object CoreTerms {
  implicit def coreTerm[L <: LA, F[_]](implicit I: InjectK[CoreTerm[L, ?], F]): CoreTerms[L, F] =
    new CoreTerms[L, F]
}
