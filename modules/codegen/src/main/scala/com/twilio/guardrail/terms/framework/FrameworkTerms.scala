package com.twilio.guardrail
package terms.framework

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.languages.LA

class FrameworkTerms[L <: LA, F[_]](implicit I: InjectK[FrameworkTerm[L, ?], F]) {
  def getFrameworkImports(tracing: Boolean): Free[F, List[L#Import]] =
    Free.inject[FrameworkTerm[L, ?], F](GetFrameworkImports[L](tracing))
  def getFrameworkImplicits(): Free[F, (L#TermName, L#ObjectDefinition)] =
    Free.inject[FrameworkTerm[L, ?], F](GetFrameworkImplicits[L]())
  def getGeneratorSettings(): Free[F, GeneratorSettings[L]] =
    Free.inject[FrameworkTerm[L, ?], F](GetGeneratorSettings[L]())
  def lookupStatusCode(key: String): Free[F, (Int, L#TermName)] =
    Free.inject[FrameworkTerm[L, ?], F](LookupStatusCode(key))
}

object FrameworkTerms {
  implicit def serverTerms[L <: LA, F[_]](implicit I: InjectK[FrameworkTerm[L, ?], F]): FrameworkTerms[L, F] =
    new FrameworkTerms[L, F]
}
