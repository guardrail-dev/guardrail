package dev.guardrail.terms.protocol

import cats.Monad

import dev.guardrail.core.SupportDefinition
import dev.guardrail.languages.LA
import dev.guardrail.terms.CollectionsLibTerms

abstract class ProtocolSupportTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) { self =>
  def MonadF: Monad[F]
  def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]): F[List[PropMeta[L]]]
  def staticProtocolImports(pkgName: List[String]): F[List[L#Import]]
  def protocolImports(): F[List[L#Import]]
  def packageObjectImports(): F[List[L#Import]]
  def packageObjectContents(): F[List[L#Statement]]
  def implicitsObject(): F[Option[(L#TermName, L#ObjectDefinition)]]
  def generateSupportDefinitions(): F[List[SupportDefinition[L]]]

  def copy(
      MonadF: Monad[F] = self.MonadF,
      extractConcreteTypes: Either[String, List[PropMeta[L]]] => F[List[PropMeta[L]]] = self.extractConcreteTypes,
      staticProtocolImports: ((List[String]) => F[List[L#Import]]) = self.staticProtocolImports _,
      protocolImports: (() => F[List[L#Import]]) = self.protocolImports _,
      packageObjectImports: (() => F[List[L#Import]]) = self.packageObjectImports _,
      packageObjectContents: (() => F[List[L#Statement]]) = self.packageObjectContents _,
      implicitsObject: () => F[Option[(L#TermName, L#ObjectDefinition)]] = self.implicitsObject _,
      generateSupportDefinitions: (() => F[List[SupportDefinition[L]]]) = self.generateSupportDefinitions _
  ): ProtocolSupportTerms[L, F] = {
    val newMonadF                     = MonadF
    val newExtractConcreteTypes       = extractConcreteTypes
    val newStaticProtocolImports      = staticProtocolImports
    val newProtocolImports            = protocolImports
    val newPackageObjectImports       = packageObjectImports
    val newPackageObjectContents      = packageObjectContents
    val newImplicitsObject            = implicitsObject
    val newGenerateSupportDefinitions = generateSupportDefinitions

    new ProtocolSupportTerms[L, F] {
      def MonadF                                                          = newMonadF
      def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]) = newExtractConcreteTypes(models)
      def staticProtocolImports(pkgName: List[String])                    = newStaticProtocolImports(pkgName)
      def protocolImports()                                               = newProtocolImports()
      def packageObjectImports()                                          = newPackageObjectImports()
      def packageObjectContents()                                         = newPackageObjectContents()
      def implicitsObject()                                               = newImplicitsObject()
      def generateSupportDefinitions()                                    = newGenerateSupportDefinitions()
    }
  }
}
