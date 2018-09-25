package com.twilio.guardrail.protocol.terms.protocol
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.{ ProtocolParameter, SupperClass }

import scala.meta.{ Defn, Stat, Term }

/**
  * Protocol for Polymorphic models
  */
sealed trait PolyProtocolTerm[T]

case class RenderSealedTrait(className: String, terms: List[Term.Param], discriminator: String, parents: List[SupperClass] = Nil)
    extends PolyProtocolTerm[Defn.Trait]

case class EncodeADT(clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[Stat]

case class DecodeADT(clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[Stat]

case class RenderADTCompanion(clsName: String, needCamelSnakeConversion: Boolean, discriminator: String, encoder: Stat, decoder: Stat)
    extends PolyProtocolTerm[Defn.Object]

class PolyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]) {
  def renderSealedTrait(
      className: String,
      terms: List[Term.Param],
      discriminator: String,
      parents: List[SupperClass] = Nil
  ): Free[F, Defn.Trait] =
    Free.inject[PolyProtocolTerm, F](RenderSealedTrait(className, terms, discriminator, parents))

  def encodeADT(clsName: String, children: List[String] = Nil): Free[F, Stat] =
    Free.inject[PolyProtocolTerm, F](EncodeADT(clsName, children))

  def decodeADT(clsName: String, children: List[String] = Nil): Free[F, Stat] =
    Free.inject[PolyProtocolTerm, F](DecodeADT(clsName, children))

  def renderADTCompanion(
      clsName: String,
      needCamelSnakeConversion: Boolean,
      discriminator: String,
      encoder: Stat,
      decoder: Stat
  ): Free[F, Defn.Object] =
    Free.inject[PolyProtocolTerm, F](RenderADTCompanion(clsName, needCamelSnakeConversion, discriminator, encoder, decoder))

}

object PolyProtocolTerms {
  implicit def polyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]): PolyProtocolTerms[F] =
    new PolyProtocolTerms[F]()
}
