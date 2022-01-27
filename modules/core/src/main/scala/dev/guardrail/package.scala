package dev

import cats.Monad
import dev.guardrail.languages.LA
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, CoreTerms, LanguageTerms, ProtocolTerms, SwaggerTerms }

package guardrail {
  trait MonadChain8 {
    implicit def monadForCollectionsLib[L <: LA, F[_]](implicit ev: CollectionsLibTerms[L, F]): Monad[F] = ev.MonadF
  }
  trait MonadChain7 extends MonadChain8 {
    implicit def monadForProtocolTerms[L <: LA, F[_]](implicit ev: ProtocolTerms[L, F]): Monad[F] = ev.MonadF
  }
  trait MonadChain6 extends MonadChain7 {
    implicit def monadForServerTerms[L <: LA, F[_]](implicit ev: ServerTerms[L, F]): Monad[F] = ev.MonadF
  }
  trait MonadChain5 extends MonadChain6 {
    implicit def monadForFrameworkTerms[L <: LA, F[_]](implicit ev: FrameworkTerms[L, F]): Monad[F] = ev.MonadF
  }
  trait MonadChain4 extends MonadChain5 {
    implicit def monadForSwagger[L <: LA, F[_]](implicit ev: SwaggerTerms[L, F]): Monad[F] = ev.MonadF
  }
  trait MonadChain3 extends MonadChain4 {
    implicit def monadForLanguage[L <: LA, F[_]](implicit ev: LanguageTerms[L, F]): Monad[F] = ev.MonadF
  }
  trait MonadChain2 extends MonadChain3 {
    implicit def monadForCore[L <: LA, F[_]](implicit ev: CoreTerms[L, F]): Monad[F] = ev.MonadF
  }
  trait MonadChain1 extends MonadChain2 {
    implicit def monadForClient[L <: LA, F[_]](implicit ev: ClientTerms[L, F]): Monad[F] = ev.MonadF
  }
}

package object guardrail extends MonadChain1 with CollectionsSyntax
