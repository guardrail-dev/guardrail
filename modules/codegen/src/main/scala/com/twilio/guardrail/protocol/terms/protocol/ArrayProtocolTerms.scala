package com.twilio.guardrail.protocol.terms.protocol

import cats.Monad
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.SwaggerUtil

abstract class ArrayProtocolTerms[L <: LA, F[_]] {
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
