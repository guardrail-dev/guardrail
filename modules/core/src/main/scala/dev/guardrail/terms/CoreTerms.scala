package dev.guardrail.terms

import cats.data.NonEmptyList
import dev.guardrail._
import dev.guardrail.languages.LA
import dev.guardrail.generators.Framework

abstract class CoreTerms[L <: LA, F[_]] { self =>
  def getDefaultFramework: F[Option[String]]
  def extractGenerator(context: Context, defaultFramework: Option[String]): F[Framework[L, Target]]
  def validateArgs(parsed: List[Args]): F[NonEmptyList[Args]]
  def processArgSet(targetInterpreter: Framework[L, Target])(args: Args): F[ReadSpec[Target[List[WriteTree]]]]
  def copy(
      getDefaultFramework: F[Option[String]] = self.getDefaultFramework,
      extractGenerator: (Context, Option[String]) => F[Framework[L, Target]] = self.extractGenerator _,
      validateArgs: List[Args] => F[NonEmptyList[Args]] = self.validateArgs _,
      processArgSet: Framework[L, Target] => Args => F[ReadSpec[Target[List[WriteTree]]]] = self.processArgSet _
  ) = {
    val newGetDefaultFramework = getDefaultFramework
    val newExtractGenerator    = extractGenerator
    val newValidateArgs        = validateArgs
    val newProcessArgSet       = processArgSet

    new CoreTerms[L, F] {
      def getDefaultFramework                                                  = newGetDefaultFramework
      def extractGenerator(context: Context, defaultFramework: Option[String]) = newExtractGenerator(context, defaultFramework)
      def validateArgs(parsed: List[Args])                                     = newValidateArgs(parsed)
      def processArgSet(targetInterpreter: Framework[L, Target])(args: Args)   = newProcessArgSet(targetInterpreter)(args)
    }
  }
}
