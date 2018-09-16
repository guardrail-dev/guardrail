package com.twilio.guardrail.protocol.terms.protocol
import cats.InjectK
import cats.free.Free

import scala.meta.{ Defn, Term }

/**
  * protocol for Polymorphic objects
  */
sealed trait PolyProtocolTerm[T]

case class RenderSealedTrait(className: String, terms: List[Term.Param], discriminator: String) extends PolyProtocolTerm[Defn.Trait]

class PolyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]) {
  def renderSealedTrait(className: String, terms: List[Term.Param], discriminator: String): Free[F, Defn.Trait] =
    Free.inject[PolyProtocolTerm, F](RenderSealedTrait(className, terms, discriminator))
}

object PolyProtocolTerms {
  implicit def polyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]): PolyProtocolTerms[F] =
    new PolyProtocolTerms[F]()
}
