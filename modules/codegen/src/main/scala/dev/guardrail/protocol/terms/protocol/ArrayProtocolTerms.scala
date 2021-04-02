package dev.guardrail.protocol.terms.protocol

import cats.Monad
import dev.guardrail.languages.LA
import dev.guardrail.SwaggerUtil
import dev.guardrail.terms.CollectionsLibTerms

abstract class ArrayProtocolTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) {
  def MonadF: Monad[F]
  def extractArrayType(arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]): F[L#Type]

  def copy(
      newMonadF: Monad[F] = this.MonadF,
      newExtractArrayType: (SwaggerUtil.ResolvedType[L], List[PropMeta[L]]) => F[L#Type] = extractArrayType _
  ): ArrayProtocolTerms[L, F] = new ArrayProtocolTerms[L, F] {
    def MonadF                                                                               = newMonadF
    def extractArrayType(arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]) = newExtractArrayType(arr, concreteTypes)
  }
}
