package com.twilio.guardrail.protocol.terms.protocol

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.SuperClass
import com.twilio.guardrail.languages.LA
import io.swagger.models.{ Model, RefModel }

/**
  * Protocol for Polymorphic models
  */
sealed trait PolyProtocolTerm[L <: LA, T]

case class ExtractSuperClass[L <: LA](swagger: Model, definitions: List[(String, Model)]) extends PolyProtocolTerm[L, List[(String, Model, List[RefModel])]]

case class RenderSealedTrait[L <: LA](className: String, terms: List[L#MethodParameter], discriminator: String, parents: List[SuperClass[L]] = Nil)
    extends PolyProtocolTerm[L, L#Trait]

case class EncodeADT[L <: LA](clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[L, L#Statement]

case class DecodeADT[L <: LA](clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[L, L#Statement]

case class RenderADTCompanion[L <: LA](clsName: String, discriminator: String, encoder: L#Statement, decoder: L#Statement)
    extends PolyProtocolTerm[L, L#ObjectDefinition]

class PolyProtocolTerms[L <: LA, F[_]](implicit I: InjectK[PolyProtocolTerm[L, ?], F]) {
  def extractSuperClass(swagger: Model, definitions: List[(String, Model)]): Free[F, List[(String, Model, List[RefModel])]] =
    Free.inject[PolyProtocolTerm[L, ?], F](ExtractSuperClass(swagger, definitions))
  def renderSealedTrait(
      className: String,
      terms: List[L#MethodParameter],
      discriminator: String,
      parents: List[SuperClass[L]] = Nil
  ): Free[F, L#Trait] =
    Free.inject[PolyProtocolTerm[L, ?], F](RenderSealedTrait(className, terms, discriminator, parents))

  def encodeADT(clsName: String, children: List[String] = Nil): Free[F, L#Statement] =
    Free.inject[PolyProtocolTerm[L, ?], F](EncodeADT(clsName, children))

  def decodeADT(clsName: String, children: List[String] = Nil): Free[F, L#Statement] =
    Free.inject[PolyProtocolTerm[L, ?], F](DecodeADT(clsName, children))

  def renderADTCompanion(
      clsName: String,
      discriminator: String,
      encoder: L#Statement,
      decoder: L#Statement
  ): Free[F, L#ObjectDefinition] =
    Free.inject[PolyProtocolTerm[L, ?], F](RenderADTCompanion(clsName, discriminator, encoder, decoder))

}

object PolyProtocolTerms {
  implicit def polyProtocolTerms[L <: LA, F[_]](implicit I: InjectK[PolyProtocolTerm[L, ?], F]): PolyProtocolTerms[L, F] =
    new PolyProtocolTerms[L, F]()
}
