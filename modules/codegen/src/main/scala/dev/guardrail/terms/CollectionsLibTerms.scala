package dev.guardrail.terms

import cats.Monad
import dev.guardrail.SwaggerUtil.LazyResolvedType
import dev.guardrail.languages.LA

abstract class CollectionsLibTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]

  def vendorPrefixes(): F[List[String]]

  def liftOptionalType(value: L#Type): F[L#Type]
  def liftOptionalTerm(value: L#Term): F[L#Term]
  def liftSomeTerm(value: L#Term): F[L#Term]
  def emptyOptionalTerm(): F[L#Term]

  def arrayType(format: Option[String]): F[L#Type]
  def liftVectorType(value: L#Type, customTpe: Option[L#Type]): F[L#Type]
  def liftVectorTerm(value: L#Term): F[L#Term]
  def emptyArray(): F[L#Term]
  def embedArray(tpe: LazyResolvedType[L], customTpe: Option[L#Type]): F[LazyResolvedType[L]]

  def liftMapType(value: L#Type, customTpe: Option[L#Type]): F[L#Type]
  def emptyMap(): F[L#Term]
  def embedMap(tpe: LazyResolvedType[L], customTpe: Option[L#Type]): F[LazyResolvedType[L]]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newVendorPrefixes: () => F[List[String]] = vendorPrefixes _,
      newLiftOptionalType: L#Type => F[L#Type] = liftOptionalType,
      newLiftOptionalTerm: L#Term => F[L#Term] = liftOptionalTerm,
      newLiftSomeTerm: L#Term => F[L#Term] = liftSomeTerm,
      newEmptyOptionalTerm: () => F[L#Term] = emptyOptionalTerm _,
      newArrayType: Option[String] => F[L#Type] = arrayType,
      newLiftVectorType: (L#Type, Option[L#Type]) => F[L#Type] = liftVectorType,
      newLiftVectorTerm: L#Term => F[L#Term] = liftVectorTerm,
      newEmptyArray: () => F[L#Term] = emptyArray _,
      newEmbedArray: (LazyResolvedType[L], Option[L#Type]) => F[LazyResolvedType[L]] = embedArray,
      newLiftMapType: (L#Type, Option[L#Type]) => F[L#Type] = liftMapType,
      newEmptyMap: () => F[L#Term] = emptyMap _,
      newEmbedMap: (LazyResolvedType[L], Option[L#Type]) => F[LazyResolvedType[L]] = embedMap
  ): CollectionsLibTerms[L, F] = new CollectionsLibTerms[L, F] {
    def MonadF: Monad[F]                                                = newMonadF
    def vendorPrefixes()                                                = newVendorPrefixes()
    def liftOptionalType(value: L#Type)                                 = newLiftOptionalType(value)
    def liftOptionalTerm(value: L#Term)                                 = newLiftOptionalTerm(value)
    def liftSomeTerm(value: L#Term)                                     = newLiftSomeTerm(value)
    def emptyOptionalTerm()                                             = newEmptyOptionalTerm()
    def arrayType(format: Option[String])                               = newArrayType(format)
    def liftVectorType(value: L#Type, customTpe: Option[L#Type])        = newLiftVectorType(value, customTpe)
    def liftVectorTerm(value: L#Term)                                   = newLiftVectorTerm(value)
    def emptyArray()                                                    = newEmptyArray()
    def embedArray(tpe: LazyResolvedType[L], customTpe: Option[L#Type]) = newEmbedArray(tpe, customTpe)
    def liftMapType(value: L#Type, customTpe: Option[L#Type])           = newLiftMapType(value, customTpe)
    def emptyMap()                                                      = newEmptyMap()
    def embedMap(tpe: LazyResolvedType[L], customTpe: Option[L#Type])   = newEmbedMap(tpe, customTpe)
  }
}
