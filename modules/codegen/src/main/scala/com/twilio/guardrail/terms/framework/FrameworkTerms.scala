package com.twilio.guardrail
package terms.framework

import cats.{ InjectK, Monad }
import cats.free.Free
import com.twilio.guardrail.languages.LA

abstract class FrameworkTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]
  def getFrameworkImports(tracing: Boolean): F[List[L#Import]]
  def getFrameworkImplicits(): F[Option[(L#TermName, L#ObjectDefinition)]]
  def getFrameworkDefinitions(tracing: Boolean): F[List[(L#TermName, L#ClassDefinition)]]
  def lookupStatusCode(key: String): F[(Int, L#TermName)]
  def fileType(format: Option[String]): F[L#Type]
  def objectType(format: Option[String]): F[L#Type]
}

object FrameworkTerms {
  implicit def serverTerms[L <: LA, F[_]](implicit I: InjectK[FrameworkTerm[L, ?], F]): FrameworkTerms[L, Free[F, ?]] = new FrameworkTerms[L, Free[F, ?]] {
    def MonadF                                                                     = Free.catsFreeMonadForFree
    def getFrameworkImports(tracing: Boolean): Free[F, List[L#Import]]             = Free.inject[FrameworkTerm[L, ?], F](GetFrameworkImports[L](tracing))
    def getFrameworkImplicits(): Free[F, Option[(L#TermName, L#ObjectDefinition)]] = Free.inject[FrameworkTerm[L, ?], F](GetFrameworkImplicits[L]())
    def getFrameworkDefinitions(tracing: Boolean): Free[F, List[(L#TermName, L#ClassDefinition)]] =
      Free.inject[FrameworkTerm[L, ?], F](GetFrameworkDefinitions[L](tracing))
    def lookupStatusCode(key: String): Free[F, (Int, L#TermName)] = Free.inject[FrameworkTerm[L, ?], F](LookupStatusCode(key))
    def fileType(format: Option[String]): Free[F, L#Type]         = Free.inject[FrameworkTerm[L, ?], F](FileType(format))
    def objectType(format: Option[String]): Free[F, L#Type]       = Free.inject[FrameworkTerm[L, ?], F](ObjectType(format))
  }
}
