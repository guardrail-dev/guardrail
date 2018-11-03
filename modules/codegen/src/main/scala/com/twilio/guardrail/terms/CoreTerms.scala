package com.twilio.guardrail
package terms

import cats.InjectK
import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.free.Free
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._

class CoreTerms[F[_]](implicit I: InjectK[CoreTerm, F]) {
  def getDefaultFramework: Free[F, String] =
    Free.inject[CoreTerm, F](GetDefaultFramework)
  def extractGenerator(context: Context): Free[F, FunctionK[CodegenApplication, Target]] =
    Free.inject[CoreTerm, F](ExtractGenerator(context))
  def extractGeneratorSettings(context: Context): Free[F, GeneratorSettings] =
    Free.inject[CoreTerm, F](ExtractGeneratorSettings(context))
  def parseArgs(args: Array[String], defaultFramework: String): Free[F, List[Args]] =
    Free.inject[CoreTerm, F](ParseArgs(args, defaultFramework))
  def validateArgs(parsed: List[Args]): Free[F, NonEmptyList[Args]] =
    Free.inject[CoreTerm, F](ValidateArgs(parsed))
  def processArgSet(targetInterpreter: FunctionK[CodegenApplication, Target])(args: Args): Free[F, ReadSwagger[Target[List[WriteTree]]]] =
    Free.inject[CoreTerm, F](ProcessArgSet(targetInterpreter, args))
}
object CoreTerms {
  implicit def coreTerm[F[_]](implicit I: InjectK[CoreTerm, F]): CoreTerms[F] =
    new CoreTerms[F]
}
