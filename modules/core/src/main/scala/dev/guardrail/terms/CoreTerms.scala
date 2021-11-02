package dev.guardrail.terms

import cats.Monad
import cats.data.NonEmptyList
import dev.guardrail._
import dev.guardrail.languages.LA
import dev.guardrail.generators.Framework

abstract class CoreTerms[L <: LA, F[_]] { self =>
  def MonadF: Monad[F]
  def getDefaultFramework: F[Option[String]]
  def extractGenerator(context: Context, defaultFramework: Option[String]): F[Framework[L, Target]]
  def validateArgs(parsed: List[Args]): F[NonEmptyList[Args]]
  def processArgSet(targetInterpreter: Framework[L, Target])(args: Args): F[ReadSwagger[Target[List[WriteTree]]]]
  def copy(
      MonadF: Monad[F] = self.MonadF,
      getDefaultFramework: F[Option[String]] = self.getDefaultFramework,
      extractGenerator: (Context, Option[String]) => F[Framework[L, Target]] = self.extractGenerator _,
      validateArgs: List[Args] => F[NonEmptyList[Args]] = self.validateArgs _,
      processArgSet: Framework[L, Target] => Args => F[ReadSwagger[Target[List[WriteTree]]]] = self.processArgSet _
  ) = {
    val newMonadF              = MonadF
    val newGetDefaultFramework = getDefaultFramework
    val newExtractGenerator    = extractGenerator
    val newValidateArgs        = validateArgs
    val newProcessArgSet       = processArgSet

    new CoreTerms[L, F] {
      def MonadF                                                               = newMonadF
      def getDefaultFramework                                                  = newGetDefaultFramework
      def extractGenerator(context: Context, defaultFramework: Option[String]) = newExtractGenerator(context, defaultFramework)
      def validateArgs(parsed: List[Args])                                     = newValidateArgs(parsed)
      def processArgSet(targetInterpreter: Framework[L, Target])(args: Args)   = newProcessArgSet(targetInterpreter)(args)
    }
  }
}
object CoreTerms {
  implicit def coreTerm[L <: LA, F[_]](implicit ev: CoreTerms[L, F]): CoreTerms[L, F] = ev
}
