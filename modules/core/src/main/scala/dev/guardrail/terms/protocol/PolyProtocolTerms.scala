package dev.guardrail.terms.protocol

import cats.Monad
import io.swagger.v3.oas.models.media.{ ComposedSchema, Schema }

import dev.guardrail.core.Tracker
import dev.guardrail.languages.LA
import dev.guardrail.terms.CollectionsLibTerms

/**
  * Protocol for Polymorphic models
  */
abstract class PolyProtocolTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) { self =>
  def MonadF: Monad[F]
  def extractSuperClass(
      swagger: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])]
  ): F[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]]
  def renderSealedTrait(
      className: String,
      params: List[ProtocolParameter[L]],
      discriminator: Discriminator[L],
      parents: List[SuperClass[L]] = Nil,
      children: List[String] = Nil
  ): F[L#Trait]
  def encodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil): F[Option[L#ValueDefinition]]
  def decodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil): F[Option[L#ValueDefinition]]
  def renderADTStaticDefns(
      clsName: String,
      discriminator: Discriminator[L],
      encoder: Option[L#ValueDefinition],
      decoder: Option[L#ValueDefinition]
  ): F[StaticDefns[L]]

  def copy(
      MonadF: Monad[F] = self.MonadF,
      extractSuperClass: (Tracker[ComposedSchema], List[(String, Tracker[Schema[_]])]) => F[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]] =
        self.extractSuperClass _,
      renderSealedTrait: (String, List[ProtocolParameter[L]], Discriminator[L], List[SuperClass[L]], List[String]) => F[L#Trait] = self.renderSealedTrait _,
      encodeADT: (String, Discriminator[L], List[String]) => F[Option[L#ValueDefinition]] = self.encodeADT _,
      decodeADT: (String, Discriminator[L], List[String]) => F[Option[L#ValueDefinition]] = self.decodeADT _,
      renderADTStaticDefns: (String, Discriminator[L], Option[L#ValueDefinition], Option[L#ValueDefinition]) => F[StaticDefns[L]] = self.renderADTStaticDefns _
  ) = {
    val newMonadF               = MonadF
    val newExtractSuperClass    = extractSuperClass
    val newRenderSealedTrait    = renderSealedTrait
    val newEncodeADT            = encodeADT
    val newDecodeADT            = decodeADT
    val newRenderADTStaticDefns = renderADTStaticDefns

    new PolyProtocolTerms[L, F] {
      def MonadF                                                                                               = newMonadF
      def extractSuperClass(swagger: Tracker[ComposedSchema], definitions: List[(String, Tracker[Schema[_]])]) = newExtractSuperClass(swagger, definitions)
      def renderSealedTrait(
          className: String,
          params: List[ProtocolParameter[L]],
          discriminator: Discriminator[L],
          parents: List[SuperClass[L]] = Nil,
          children: List[String] = Nil
      )                                                                                             = newRenderSealedTrait(className, params, discriminator, parents, children)
      def encodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil) = newEncodeADT(clsName, discriminator, children)
      def decodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil) = newDecodeADT(clsName, discriminator, children)
      def renderADTStaticDefns(clsName: String, discriminator: Discriminator[L], encoder: Option[L#ValueDefinition], decoder: Option[L#ValueDefinition]) =
        newRenderADTStaticDefns(clsName, discriminator, encoder, decoder)
    }
  }
}
