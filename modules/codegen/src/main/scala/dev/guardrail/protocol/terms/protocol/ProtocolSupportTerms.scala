package dev.guardrail.protocol.terms.protocol

import cats.Monad
import dev.guardrail.SupportDefinition
import dev.guardrail.languages.LA
import dev.guardrail.terms.CollectionsLibTerms

abstract class ProtocolSupportTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) {
  def MonadF: Monad[F]
  def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]): F[List[PropMeta[L]]]
  def staticProtocolImports(pkgName: List[String]): F[List[L#Import]]
  def protocolImports(): F[List[L#Import]]
  def packageObjectImports(): F[List[L#Import]]
  def packageObjectContents(): F[List[L#Statement]]
  def implicitsObject(): F[Option[(L#TermName, L#ObjectDefinition)]]
  def generateSupportDefinitions(): F[List[SupportDefinition[L]]]

  def copy(
      newMonadF: Monad[F] = this.MonadF,
      newExtractConcreteTypes: Either[String, List[PropMeta[L]]] => F[List[PropMeta[L]]] = this.extractConcreteTypes,
      newStaticProcolImports: ((List[String]) => F[List[L#Import]]) = this.staticProtocolImports _,
      newProtocolImports: (() => F[List[L#Import]]) = this.protocolImports _,
      newPackageObjectImports: (() => F[List[L#Import]]) = this.packageObjectImports _,
      newPackageObjectContents: (() => F[List[L#Statement]]) = this.packageObjectContents _,
      newImplicitsObject: () => F[Option[(L#TermName, L#ObjectDefinition)]] = this.implicitsObject _,
      newGenerateSupportDefinitions: (() => F[List[SupportDefinition[L]]]) = this.generateSupportDefinitions _
  ): ProtocolSupportTerms[L, F] = new ProtocolSupportTerms[L, F] {
    def MonadF                                                          = newMonadF
    def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]) = newExtractConcreteTypes(models)
    def staticProtocolImports(pkgName: List[String])                    = newStaticProcolImports(pkgName)
    def protocolImports()                                               = newProtocolImports()
    def packageObjectImports()                                          = newPackageObjectImports()
    def packageObjectContents()                                         = newPackageObjectContents()
    def implicitsObject()                                               = newImplicitsObject()
    def generateSupportDefinitions()                                    = newGenerateSupportDefinitions()
  }
}
