package dev.guardrail
package terms

import cats.Monad
import cats.data.NonEmptyList
import dev.guardrail.languages.LA
import dev.guardrail.generators.Framework

abstract class CoreTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]
  def getDefaultFramework: F[Option[String]]
  def extractGenerator(context: Context, defaultFramework: Option[String]): F[Framework[L, Target]]
  def parseArgs(args: Array[String]): F[List[Args]]
  def validateArgs(parsed: List[Args]): F[NonEmptyList[Args]]
  def processArgSet(targetInterpreter: Framework[L, Target])(args: Args): F[ReadSwagger[Target[List[WriteTree]]]]
  def copy(
      newMonadF: Monad[F] = MonadF,
      newGetDefaultFramework: F[Option[String]] = getDefaultFramework,
      newExtractGenerator: (Context, Option[String]) => F[Framework[L, Target]] = extractGenerator _,
      newParseArgs: Array[String] => F[List[Args]] = parseArgs _,
      newValidateArgs: List[Args] => F[NonEmptyList[Args]] = validateArgs _,
      newProcessArgSet: Framework[L, Target] => Args => F[ReadSwagger[Target[List[WriteTree]]]] = processArgSet _
  ) = new CoreTerms[L, F] {
    def MonadF                                                               = newMonadF
    def getDefaultFramework                                                  = newGetDefaultFramework
    def extractGenerator(context: Context, defaultFramework: Option[String]) = newExtractGenerator(context, defaultFramework)
    def parseArgs(args: Array[String])                                       = newParseArgs(args)
    def validateArgs(parsed: List[Args])                                     = newValidateArgs(parsed)
    def processArgSet(targetInterpreter: Framework[L, Target])(args: Args)   = newProcessArgSet(targetInterpreter)(args)
  }
}
object CoreTerms {
  implicit def coreTerm[L <: LA, F[_]](implicit ev: CoreTerms[L, F]): CoreTerms[L, F] = ev
}
