package com.twilio.guardrail.protocol.terms.protocol

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.{ Discriminator, ProtocolParameter, StaticDefns, SuperClass }
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.LA
import io.swagger.v3.oas.models.media.{ ComposedSchema, Schema }

/**
  * Protocol for Polymorphic models
  */
sealed trait PolyProtocolTerm[L <: LA, T]

case class ExtractSuperClass[L <: LA](swagger: Tracker[ComposedSchema], definitions: List[(String, Tracker[Schema[_]])])
    extends PolyProtocolTerm[L, List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]]

case class RenderSealedTrait[L <: LA](
    className: String,
    params: List[ProtocolParameter[L]],
    discriminator: Discriminator[L],
    parents: List[SuperClass[L]] = Nil,
    children: List[String] = Nil
) extends PolyProtocolTerm[L, L#Trait]

case class EncodeADT[L <: LA](clsName: String, discriminator: Discriminator[L], children: List[String] = Nil)
    extends PolyProtocolTerm[L, Option[L#ValueDefinition]]

case class DecodeADT[L <: LA](clsName: String, discriminator: Discriminator[L], children: List[String] = Nil)
    extends PolyProtocolTerm[L, Option[L#ValueDefinition]]

case class RenderADTStaticDefns[L <: LA](
    clsName: String,
    discriminator: Discriminator[L],
    encoder: Option[L#ValueDefinition],
    decoder: Option[L#ValueDefinition]
) extends PolyProtocolTerm[L, StaticDefns[L]]

class PolyProtocolTerms[L <: LA, F[_]](implicit I: InjectK[PolyProtocolTerm[L, ?], F]) {
  def extractSuperClass(
      swagger: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])]
  ): Free[F, List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]] =
    Free.inject[PolyProtocolTerm[L, ?], F](ExtractSuperClass(swagger, definitions))
  def renderSealedTrait(
      className: String,
      params: List[ProtocolParameter[L]],
      discriminator: Discriminator[L],
      parents: List[SuperClass[L]] = Nil,
      children: List[String] = Nil
  ): Free[F, L#Trait] =
    Free.inject[PolyProtocolTerm[L, ?], F](RenderSealedTrait(className, params, discriminator, parents, children))

  def encodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil): Free[F, Option[L#ValueDefinition]] =
    Free.inject[PolyProtocolTerm[L, ?], F](EncodeADT(clsName, discriminator, children))

  def decodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil): Free[F, Option[L#ValueDefinition]] =
    Free.inject[PolyProtocolTerm[L, ?], F](DecodeADT(clsName, discriminator, children))

  def renderADTStaticDefns(
      clsName: String,
      discriminator: Discriminator[L],
      encoder: Option[L#ValueDefinition],
      decoder: Option[L#ValueDefinition]
  ): Free[F, StaticDefns[L]] =
    Free.inject[PolyProtocolTerm[L, ?], F](RenderADTStaticDefns(clsName, discriminator, encoder, decoder))

}

object PolyProtocolTerms {
  implicit def polyProtocolTerms[L <: LA, F[_]](implicit I: InjectK[PolyProtocolTerm[L, ?], F]): PolyProtocolTerms[L, F] =
    new PolyProtocolTerms[L, F]()
}
