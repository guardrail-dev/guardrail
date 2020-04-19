package com.twilio.guardrail.protocol.terms.protocol

import cats.{ InjectK, Monad }
import cats.free.Free
import com.twilio.guardrail.languages.LA

abstract class ProtocolSupportTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]
  def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]): F[List[PropMeta[L]]]
  def protocolImports(): F[List[L#Import]]
  def packageObjectImports(): F[List[L#Import]]
  def packageObjectContents(): F[List[L#ValueDefinition]]

  def copy(
      newMonadF: Monad[F] = this.MonadF,
      newExtractConcreteTypes: Either[String, List[PropMeta[L]]] => F[List[PropMeta[L]]] = this.extractConcreteTypes,
      newProtocolImports: (() => F[List[L#Import]]) = this.protocolImports _,
      newPackageObjectImports: (() => F[List[L#Import]]) = this.packageObjectImports _,
      newPackageObjectContents: (() => F[List[L#ValueDefinition]]) = this.packageObjectContents _
  ): ProtocolSupportTerms[L, F] = new ProtocolSupportTerms[L, F] {
    def MonadF                                                          = newMonadF
    def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]) = newExtractConcreteTypes(models)
    def protocolImports()                                               = newProtocolImports()
    def packageObjectImports()                                          = newPackageObjectImports()
    def packageObjectContents()                                         = newPackageObjectContents()
  }
}
object ProtocolSupportTerms {
  implicit def protocolSupportTerms[L <: LA, F[_]](implicit I: InjectK[ProtocolSupportTerm[L, ?], F]): ProtocolSupportTerms[L, Free[F, ?]] =
    new ProtocolSupportTerms[L, Free[F, ?]] {
      def MonadF = Free.catsFreeMonadForFree
      def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]): Free[F, List[PropMeta[L]]] =
        Free.inject[ProtocolSupportTerm[L, ?], F](ExtractConcreteTypes(models))
      def protocolImports(): Free[F, List[L#Import]]                = Free.inject[ProtocolSupportTerm[L, ?], F](ProtocolImports())
      def packageObjectImports(): Free[F, List[L#Import]]           = Free.inject[ProtocolSupportTerm[L, ?], F](PackageObjectImports())
      def packageObjectContents(): Free[F, List[L#ValueDefinition]] = Free.inject[ProtocolSupportTerm[L, ?], F](PackageObjectContents())
    }
}
