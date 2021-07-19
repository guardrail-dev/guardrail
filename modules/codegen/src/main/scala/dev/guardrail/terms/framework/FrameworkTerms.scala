package dev.guardrail
package terms.framework

import cats.Monad
import dev.guardrail.languages.LA
import dev.guardrail.terms.CollectionsLibTerms

abstract class FrameworkTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) {
  def MonadF: Monad[F]
  def getFrameworkImports(tracing: Boolean): F[List[L#Import]]
  def getFrameworkImplicits(): F[Option[(L#TermName, L#ObjectDefinition)]]
  def getFrameworkDefinitions(tracing: Boolean): F[List[(L#TermName, List[L#Definition])]]
  def lookupStatusCode(key: String): F[(Int, L#TermName)]
  def fileType(format: Option[String]): F[L#Type]
  def objectType(format: Option[String]): F[L#Type]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newGetFrameworkImports: Boolean => F[List[L#Import]] = getFrameworkImports _,
      newGetFrameworkImplicits: () => F[Option[(L#TermName, L#ObjectDefinition)]] = getFrameworkImplicits _,
      newGetFrameworkDefinitions: Boolean => F[List[(L#TermName, List[L#Definition])]] = getFrameworkDefinitions _,
      newLookupStatusCode: String => F[(Int, L#TermName)] = lookupStatusCode _,
      newFileType: Option[String] => F[L#Type] = fileType _,
      newObjectType: Option[String] => F[L#Type] = objectType _
  ) = new FrameworkTerms[L, F] {
    def MonadF                                    = newMonadF
    def getFrameworkImports(tracing: Boolean)     = newGetFrameworkImports(tracing)
    def getFrameworkImplicits()                   = newGetFrameworkImplicits()
    def getFrameworkDefinitions(tracing: Boolean) = newGetFrameworkDefinitions(tracing)
    def lookupStatusCode(key: String)             = newLookupStatusCode(key)
    def fileType(format: Option[String])          = newFileType(format)
    def objectType(format: Option[String])        = newObjectType(format)
  }
}
