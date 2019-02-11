package com.twilio.guardrail.protocol.terms.protocol

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.{ StaticDefns, SuperClass }
import com.twilio.guardrail.languages.LA
import io.swagger.v3.oas.models.media.{ ComposedSchema, Schema }

/**
  * Protocol for Polymorphic models
  */
sealed trait PolyProtocolTerm[L <: LA, T]

case class ExtractSuperClass[L <: LA](swagger: ComposedSchema, definitions: List[(String, Schema[_])])
    extends PolyProtocolTerm[L, List[(String, Schema[_], List[Schema[_]])]]

case class RenderSealedTrait[L <: LA](className: String, terms: List[L#MethodParameter], discriminator: String, parents: List[SuperClass[L]] = Nil)
    extends PolyProtocolTerm[L, L#Trait]

case class EncodeADT[L <: LA](clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[L, L#ValueDefinition]

case class DecodeADT[L <: LA](clsName: String, children: List[String] = Nil) extends PolyProtocolTerm[L, L#ValueDefinition]

case class RenderADTStaticDefns[L <: LA](clsName: String, discriminator: String, encoder: L#ValueDefinition, decoder: L#ValueDefinition)
    extends PolyProtocolTerm[L, StaticDefns[L]]

class PolyProtocolTerms[L <: LA, F[_]](implicit I: InjectK[PolyProtocolTerm[L, ?], F]) {
  def extractSuperClass(swagger: ComposedSchema, definitions: List[(String, Schema[_])]): Free[F, List[(String, Schema[_], List[Schema[_]])]] =
    Free.inject[PolyProtocolTerm[L, ?], F](ExtractSuperClass(swagger, definitions))
  def renderSealedTrait(
      className: String,
      terms: List[L#MethodParameter],
      discriminator: String,
      parents: List[SuperClass[L]] = Nil
  ): Free[F, L#Trait] =
    Free.inject[PolyProtocolTerm[L, ?], F](RenderSealedTrait(className, terms, discriminator, parents))

  def encodeADT(clsName: String, children: List[String] = Nil): Free[F, L#ValueDefinition] =
    Free.inject[PolyProtocolTerm[L, ?], F](EncodeADT(clsName, children))

  def decodeADT(clsName: String, children: List[String] = Nil): Free[F, L#ValueDefinition] =
    Free.inject[PolyProtocolTerm[L, ?], F](DecodeADT(clsName, children))

  def renderADTStaticDefns(
      clsName: String,
      discriminator: String,
      encoder: L#ValueDefinition,
      decoder: L#ValueDefinition
  ): Free[F, StaticDefns[L]] =
    Free.inject[PolyProtocolTerm[L, ?], F](RenderADTStaticDefns(clsName, discriminator, encoder, decoder))

}

object PolyProtocolTerms {
  implicit def polyProtocolTerms[L <: LA, F[_]](implicit I: InjectK[PolyProtocolTerm[L, ?], F]): PolyProtocolTerms[L, F] =
    new PolyProtocolTerms[L, F]()
}
