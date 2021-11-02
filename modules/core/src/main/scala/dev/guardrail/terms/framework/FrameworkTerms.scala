package dev.guardrail.terms.framework

import cats.Monad

import dev.guardrail.languages.LA
import dev.guardrail.terms.CollectionsLibTerms

abstract class FrameworkTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) { self =>
  def MonadF: Monad[F]
  def getFrameworkImports(tracing: Boolean): F[List[L#Import]]
  def getFrameworkImplicits(): F[Option[(L#TermName, L#ObjectDefinition)]]
  def getFrameworkDefinitions(tracing: Boolean): F[List[(L#TermName, List[L#Definition])]]
  def lookupStatusCode(key: String): F[(Int, L#TermName)]
  def fileType(format: Option[String]): F[L#Type]
  def objectType(format: Option[String]): F[L#Type]

  def copy(
      MonadF: Monad[F] = self.MonadF,
      getFrameworkImports: Boolean => F[List[L#Import]] = self.getFrameworkImports _,
      getFrameworkImplicits: () => F[Option[(L#TermName, L#ObjectDefinition)]] = self.getFrameworkImplicits _,
      getFrameworkDefinitions: Boolean => F[List[(L#TermName, List[L#Definition])]] = self.getFrameworkDefinitions _,
      lookupStatusCode: String => F[(Int, L#TermName)] = self.lookupStatusCode _,
      fileType: Option[String] => F[L#Type] = self.fileType _,
      objectType: Option[String] => F[L#Type] = self.objectType _
  ) = {
    val newMonadF                  = MonadF
    val newGetFrameworkImports     = getFrameworkImports
    val newGetFrameworkImplicits   = getFrameworkImplicits
    val newGetFrameworkDefinitions = getFrameworkDefinitions
    val newLookupStatusCode        = lookupStatusCode
    val newFileType                = fileType
    val newObjectType              = objectType

    new FrameworkTerms[L, F] {
      def MonadF                                    = newMonadF
      def getFrameworkImports(tracing: Boolean)     = newGetFrameworkImports(tracing)
      def getFrameworkImplicits()                   = newGetFrameworkImplicits()
      def getFrameworkDefinitions(tracing: Boolean) = newGetFrameworkDefinitions(tracing)
      def lookupStatusCode(key: String)             = newLookupStatusCode(key)
      def fileType(format: Option[String])          = newFileType(format)
      def objectType(format: Option[String])        = newObjectType(format)
    }
  }
}
