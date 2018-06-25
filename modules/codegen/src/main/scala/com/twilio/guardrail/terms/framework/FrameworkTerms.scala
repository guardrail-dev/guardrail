package com.twilio.guardrail
package terms.framework

import cats.InjectK
import cats.free.Free

import scala.meta._
import com.twilio.guardrail.generators.GeneratorSettings

class FrameworkTerms[F[_]](implicit I: InjectK[FrameworkTerm, F]) {
  def getFrameworkImports(tracing: Boolean): Free[F, List[Import]] =
    Free.inject(GetFrameworkImports(tracing))
  def getFrameworkImplicits(generatorSettings: GeneratorSettings): Free[F, Defn.Object] =
    Free.inject(GetFrameworkImplicits(generatorSettings))
  def getGeneratorSettings(): Free[F, GeneratorSettings] =
    Free.inject(GetGeneratorSettings())
}

object FrameworkTerms {
  implicit def serverTerms[F[_]](implicit I: InjectK[FrameworkTerm, F]): FrameworkTerms[F] =
    new FrameworkTerms[F]
}
