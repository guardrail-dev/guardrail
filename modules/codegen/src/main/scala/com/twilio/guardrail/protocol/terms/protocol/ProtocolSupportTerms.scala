package com.twilio.guardrail.protocol.terms.protocol

import cats.Monad
import com.twilio.guardrail.languages.LA

abstract class ProtocolSupportTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]
  def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]): F[List[PropMeta[L]]]
  def protocolImports(): F[List[L#Import]]
  def packageObjectImports(): F[List[L#Import]]
  def packageObjectContents(): F[List[L#Statement]]

  def copy(
      newMonadF: Monad[F] = this.MonadF,
      newExtractConcreteTypes: Either[String, List[PropMeta[L]]] => F[List[PropMeta[L]]] = this.extractConcreteTypes,
      newProtocolImports: (() => F[List[L#Import]]) = this.protocolImports _,
      newPackageObjectImports: (() => F[List[L#Import]]) = this.packageObjectImports _,
      newPackageObjectContents: (() => F[List[L#Statement]]) = this.packageObjectContents _
  ): ProtocolSupportTerms[L, F] = new ProtocolSupportTerms[L, F] {
    def MonadF                                                          = newMonadF
    def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]) = newExtractConcreteTypes(models)
    def protocolImports()                                               = newProtocolImports()
    def packageObjectImports()                                          = newPackageObjectImports()
    def packageObjectContents()                                         = newPackageObjectContents()
  }
}
