package com.twilio.guardrail.protocol.terms.protocol

import cats.{ InjectK, Monad }
import cats.arrow.FunctionK
import cats.free.Free
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.SwaggerUtil

abstract class ArrayProtocolTerms[L <: LA, F[_]] extends FunctionK[ArrayProtocolTerm[L, ?], F] {
  def MonadF: Monad[F]
  def extractArrayType(arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]): F[L#Type]

  def copy(
      newMonadF: Monad[F] = this.MonadF,
      newExtractArrayType: (SwaggerUtil.ResolvedType[L], List[PropMeta[L]]) => F[L#Type] = extractArrayType _
  ): ArrayProtocolTerms[L, F] = new ArrayProtocolTerms[L, F] {
    def MonadF                                                                               = newMonadF
    def extractArrayType(arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]) = newExtractArrayType(arr, concreteTypes)
  }

  def apply[T](term: ArrayProtocolTerm[L, T]): F[T] = term match {
    case ExtractArrayType(arr, concreteTypes) => extractArrayType(arr, concreteTypes)
  }
}

object ArrayProtocolTerms {
  implicit def arrayProtocolTerms[L <: LA, F[_]](implicit I: InjectK[ArrayProtocolTerm[L, ?], F]): ArrayProtocolTerms[L, Free[F, ?]] =
    new ArrayProtocolTerms[L, Free[F, ?]] {
      def MonadF = Free.catsFreeMonadForFree
      def extractArrayType(arr: SwaggerUtil.ResolvedType[L], concreteTypes: List[PropMeta[L]]): Free[F, L#Type] =
        Free.inject[ArrayProtocolTerm[L, ?], F](ExtractArrayType[L](arr, concreteTypes))
    }
}
