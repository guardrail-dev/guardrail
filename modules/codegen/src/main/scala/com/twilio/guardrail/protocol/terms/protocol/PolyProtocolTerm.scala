package com.twilio.guardrail.protocol.terms.protocol
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.ProtocolParameter

import scala.meta.{ Defn, Stat, Term }

/**
  * protocol for Polymorphic objects
  */
sealed trait PolyProtocolTerm[T]

case class RenderSealedTrait(className: String, terms: List[Term.Param], discriminator: String)           extends PolyProtocolTerm[Defn.Trait]
case class EncodeADT(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]) extends PolyProtocolTerm[Stat]

case class DecodeADT(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]) extends PolyProtocolTerm[Stat]

case class RenderDiscriminator(discriminator: String) extends PolyProtocolTerm[Stat]

case class RenderADTCompanion(clsName: String, discriminator: Stat, encoder: Stat, decoder: Stat) extends PolyProtocolTerm[Defn.Object]

class PolyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]) {
  def renderSealedTrait(className: String, terms: List[Term.Param], discriminator: String): Free[F, Defn.Trait] =
    Free.inject[PolyProtocolTerm, F](RenderSealedTrait(className, terms, discriminator))

  def encodeADT(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]): Free[F, Stat] =
    Free.inject[PolyProtocolTerm, F](EncodeADT(clsName, needCamelSnakeConversion, params))

  def decodeADT(clsName: String, needCamelSnakeConversion: Boolean, params: List[ProtocolParameter]): Free[F, Stat] =
    Free.inject[PolyProtocolTerm, F](DecodeADT(clsName, needCamelSnakeConversion, params))

  def renderDiscriminator(discriminator: String): Free[F, Stat] =
    Free.inject[PolyProtocolTerm, F](RenderDiscriminator(discriminator))

  def renderADTCompanion(clsName: String, discriminator: Stat, encoder: Stat, decoder: Stat): Free[F, Defn.Object] =
    Free.inject[PolyProtocolTerm, F](RenderADTCompanion(clsName, discriminator, encoder, decoder))

}

object PolyProtocolTerms {
  implicit def polyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]): PolyProtocolTerms[F] =
    new PolyProtocolTerms[F]()
}
