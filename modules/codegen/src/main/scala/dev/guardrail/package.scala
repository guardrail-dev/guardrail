package dev

import cats.Monad
import dev.guardrail.languages.LA
import dev.guardrail.protocol.terms.client.ClientTerms
import dev.guardrail.protocol.terms.protocol._
import dev.guardrail.protocol.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, CoreTerms, LanguageTerms, SwaggerTerms }

package guardrail {
  case class CodegenDefinitions[L <: LA](
      clients: List[Client[L]],
      servers: List[Server[L]],
      supportDefinitions: List[SupportDefinition[L]],
      frameworksImplicits: Option[(L#TermName, L#ObjectDefinition)]
  )
}

trait MonadChain12 {
  implicit def monadForCollectionsLib[L <: LA, F[_]](implicit ev: CollectionsLibTerms[L, F]): Monad[F] = ev.MonadF
}
trait MonadChain11 extends MonadChain12 {
  implicit def monadForProtocolSupportTerms[L <: LA, F[_]](implicit ev: ProtocolSupportTerms[L, F]): Monad[F] = ev.MonadF
}
trait MonadChain10 extends MonadChain11 {
  implicit def monadForServerTerms[L <: LA, F[_]](implicit ev: ServerTerms[L, F]): Monad[F] = ev.MonadF
}
trait MonadChain9 extends MonadChain10 {
  implicit def monadForFrameworkTerms[L <: LA, F[_]](implicit ev: FrameworkTerms[L, F]): Monad[F] = ev.MonadF
}
trait MonadChain8 extends MonadChain9 {
  implicit def monadForPolyProtocolTerms[L <: LA, F[_]](implicit ev: PolyProtocolTerms[L, F]): Monad[F] = ev.MonadF
}
trait MonadChain7 extends MonadChain8 {
  implicit def monadForModelProtocolTerms[L <: LA, F[_]](implicit ev: ModelProtocolTerms[L, F]): Monad[F] = ev.MonadF
}
trait MonadChain6 extends MonadChain7 {
  implicit def monadForEnumProtocolTerms[L <: LA, F[_]](implicit ev: EnumProtocolTerms[L, F]): Monad[F] = ev.MonadF
}
trait MonadChain5 extends MonadChain6 {
  implicit def monadForArray[L <: LA, F[_]](implicit ev: ArrayProtocolTerms[L, F]): Monad[F] = ev.MonadF
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

package object guardrail extends MonadChain1
