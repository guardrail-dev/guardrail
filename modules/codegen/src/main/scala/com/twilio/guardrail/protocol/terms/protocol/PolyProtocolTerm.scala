package com.twilio.guardrail.protocol.terms.protocol
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.SuperClass
import io.swagger.models.{ Model, RefModel }

import scala.meta.{ Defn, Stat, Term }

/**
  * Protocol for Polymorphic models
  */
sealed trait PolyProtocolTerm[T]

case class ExtractSuperClass(swagger: Model, definitions: List[(String, Model)]) extends PolyProtocolTerm[List[(String, Model, List[RefModel])]]

case class RenderSealedTrait(className: String, terms: List[Term.Param], discriminator: String, parents: List[SuperClass] = Nil)
    extends PolyProtocolTerm[Defn.Trait]

case class EncodeADT(clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[Stat]

case class DecodeADT(clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[Stat]

case class RenderADTCompanion(clsName: String, discriminator: String, encoder: Stat, decoder: Stat) extends PolyProtocolTerm[Defn.Object]

class PolyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]) {
  def extractSuperClass(swagger: Model, definitions: List[(String, Model)]): Free[F, List[(String, Model, List[RefModel])]] =
    Free.inject[PolyProtocolTerm, F](ExtractSuperClass(swagger, definitions))
  def renderSealedTrait(
      className: String,
      terms: List[Term.Param],
      discriminator: String,
      parents: List[SuperClass] = Nil
  ): Free[F, Defn.Trait] =
    Free.inject[PolyProtocolTerm, F](RenderSealedTrait(className, terms, discriminator, parents))

  def encodeADT(clsName: String, children: List[String] = Nil): Free[F, Stat] =
    Free.inject[PolyProtocolTerm, F](EncodeADT(clsName, children))

  def decodeADT(clsName: String, children: List[String] = Nil): Free[F, Stat] =
    Free.inject[PolyProtocolTerm, F](DecodeADT(clsName, children))

  def renderADTCompanion(
      clsName: String,
      discriminator: String,
      encoder: Stat,
      decoder: Stat
  ): Free[F, Defn.Object] =
    Free.inject[PolyProtocolTerm, F](RenderADTCompanion(clsName, discriminator, encoder, decoder))

}

object PolyProtocolTerms {
  implicit def polyProtocolTerms[F[_]](implicit I: InjectK[PolyProtocolTerm, F]): PolyProtocolTerms[F] =
    new PolyProtocolTerms[F]()
}
