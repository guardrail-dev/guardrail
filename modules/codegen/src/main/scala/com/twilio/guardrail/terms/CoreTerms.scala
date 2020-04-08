package com.twilio.guardrail
package terms

import cats.~>
import cats.Monad
import cats.data.NonEmptyList
import com.twilio.guardrail.languages.LA

abstract class CoreTerms[L <: LA, F[_]] {
  implicit def MonadF: Monad[F]
  def getDefaultFramework: F[Option[String]]
  def extractGenerator(context: Context, defaultFramework: Option[String]): F[CodegenApplication[L, ?] ~> Target]
  def parseArgs(args: Array[String]): F[List[Args]]
  def validateArgs(parsed: List[Args]): F[NonEmptyList[Args]]
  def processArgSet(targetInterpreter: CodegenApplication[L, ?] ~> Target)(args: Args): F[ReadSwagger[Target[List[WriteTree]]]]
}
object CoreTerms {
  implicit def coreTerm[L <: LA, F[_]](implicit ev: CoreTerms[L, F]): CoreTerms[L, F] = ev
}
