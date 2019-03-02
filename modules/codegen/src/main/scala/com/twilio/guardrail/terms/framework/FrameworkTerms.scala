package com.twilio.guardrail
package terms.framework

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.languages.LA

class FrameworkTerms[L <: LA, F[_]](implicit I: InjectK[FrameworkTerm[L, ?], F]) {
  def getFrameworkImports(tracing: Boolean): Free[F, List[L#Import]] =
    Free.inject[FrameworkTerm[L, ?], F](GetFrameworkImports[L](tracing))
  def getFrameworkImplicits(): Free[F, Option[(L#TermName, L#ObjectDefinition)]] =
    Free.inject[FrameworkTerm[L, ?], F](GetFrameworkImplicits[L]())
  def getFrameworkDefinitions(): Free[F, List[(L#TermName, L#ClassDefinition)]] =
    Free.inject[FrameworkTerm[L, ?], F](GetFrameworkDefinitions[L]())
  def lookupStatusCode(key: String): Free[F, (Int, L#TermName)] =
    Free.inject[FrameworkTerm[L, ?], F](LookupStatusCode(key))
  def fileType(format: Option[String]): Free[F, L#Type] =
    Free.inject[FrameworkTerm[L, ?], F](FileType(format))
  def objectType(format: Option[String]): Free[F, L#Type] =
    Free.inject[FrameworkTerm[L, ?], F](ObjectType(format))
}

object FrameworkTerms {
  implicit def serverTerms[L <: LA, F[_]](implicit I: InjectK[FrameworkTerm[L, ?], F]): FrameworkTerms[L, F] =
    new FrameworkTerms[L, F]
}
