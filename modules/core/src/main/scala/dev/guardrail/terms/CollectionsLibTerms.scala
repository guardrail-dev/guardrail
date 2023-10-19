package dev.guardrail.terms

import dev.guardrail.core.LazyResolvedType
import dev.guardrail.languages.LA

abstract class CollectionsLibTerms[L <: LA, F[_]] { self =>
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
      vendorPrefixes: () => F[List[String]] = self.vendorPrefixes _,
      liftOptionalType: L#Type => F[L#Type] = self.liftOptionalType,
      liftOptionalTerm: L#Term => F[L#Term] = self.liftOptionalTerm,
      liftSomeTerm: L#Term => F[L#Term] = self.liftSomeTerm,
      emptyOptionalTerm: () => F[L#Term] = self.emptyOptionalTerm _,
      arrayType: Option[String] => F[L#Type] = self.arrayType,
      liftVectorType: (L#Type, Option[L#Type]) => F[L#Type] = self.liftVectorType,
      liftVectorTerm: L#Term => F[L#Term] = self.liftVectorTerm,
      emptyArray: () => F[L#Term] = self.emptyArray _,
      embedArray: (LazyResolvedType[L], Option[L#Type]) => F[LazyResolvedType[L]] = self.embedArray,
      liftMapType: (L#Type, Option[L#Type]) => F[L#Type] = self.liftMapType,
      emptyMap: () => F[L#Term] = self.emptyMap _,
      embedMap: (LazyResolvedType[L], Option[L#Type]) => F[LazyResolvedType[L]] = self.embedMap
  ): CollectionsLibTerms[L, F] = {
    val newVendorPrefixes    = vendorPrefixes
    val newLiftOptionalType  = liftOptionalType
    val newLiftOptionalTerm  = liftOptionalTerm
    val newLiftSomeTerm      = liftSomeTerm
    val newEmptyOptionalTerm = emptyOptionalTerm
    val newArrayType         = arrayType
    val newLiftVectorType    = liftVectorType
    val newLiftVectorTerm    = liftVectorTerm
    val newEmptyArray        = emptyArray
    val newEmbedArray        = embedArray
    val newLiftMapType       = liftMapType
    val newEmptyMap          = emptyMap
    val newEmbedMap          = embedMap

    new CollectionsLibTerms[L, F] {
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
}
