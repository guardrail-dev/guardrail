package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import cats.{ InjectK, Monad }
import cats.arrow.FunctionK
import cats.free.Free
import com.twilio.guardrail.StaticDefns
import com.twilio.guardrail.languages.LA

abstract class EnumProtocolTerms[L <: LA, F[_]] extends FunctionK[EnumProtocolTerm[L, ?], F] {
  def MonadF: Monad[F]
  def extractEnum(swagger: Schema[_]): F[Either[String, List[String]]]
  def renderMembers(clsName: String, elems: List[(String, L#TermName, L#TermSelect)]): F[Option[L#ObjectDefinition]]
  def encodeEnum(clsName: String): F[Option[L#ValueDefinition]]
  def decodeEnum(clsName: String): F[Option[L#ValueDefinition]]
  def renderClass(clsName: String, tpe: L#Type, elems: List[(String, L#TermName, L#TermSelect)]): F[L#ClassDefinition]
  def renderStaticDefns(
      clsName: String,
      members: Option[L#ObjectDefinition],
      accessors: List[L#TermName],
      encoder: Option[L#ValueDefinition],
      decoder: Option[L#ValueDefinition]
  ): F[StaticDefns[L]]
  def buildAccessor(clsName: String, termName: String): F[L#TermSelect]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newExtractEnum: Schema[_] => F[Either[String, List[String]]] = extractEnum _,
      newRenderMembers: (String, List[(String, L#TermName, L#TermSelect)]) => F[Option[L#ObjectDefinition]] = renderMembers _,
      newEncodeEnum: String => F[Option[L#ValueDefinition]] = encodeEnum _,
      newDecodeEnum: String => F[Option[L#ValueDefinition]] = decodeEnum _,
      newRenderClass: (String, L#Type, List[(String, L#TermName, L#TermSelect)]) => F[L#ClassDefinition] = renderClass _,
      newRenderStaticDefns: (String, Option[L#ObjectDefinition], List[L#TermName], Option[L#ValueDefinition], Option[L#ValueDefinition]) => F[StaticDefns[L]] =
        renderStaticDefns _,
      newBuildAccessor: (String, String) => F[L#TermSelect] = buildAccessor _
  ) = new EnumProtocolTerms[L, F] {
    def MonadF                                                                                     = newMonadF
    def extractEnum(swagger: Schema[_])                                                            = newExtractEnum(swagger)
    def renderMembers(clsName: String, elems: List[(String, L#TermName, L#TermSelect)])            = newRenderMembers(clsName, elems)
    def encodeEnum(clsName: String)                                                                = newEncodeEnum(clsName)
    def decodeEnum(clsName: String)                                                                = newDecodeEnum(clsName)
    def renderClass(clsName: String, tpe: L#Type, elems: List[(String, L#TermName, L#TermSelect)]) = newRenderClass(clsName, tpe, elems)
    def renderStaticDefns(
        clsName: String,
        members: Option[L#ObjectDefinition],
        accessors: List[L#TermName],
        encoder: Option[L#ValueDefinition],
        decoder: Option[L#ValueDefinition]
    )                                                    = newRenderStaticDefns(clsName, members, accessors, encoder, decoder)
    def buildAccessor(clsName: String, termName: String) = newBuildAccessor(clsName, termName)
  }

  def apply[T](term: EnumProtocolTerm[L, T]): F[T] = term match {
    case ExtractEnum(swagger)                                             => extractEnum(swagger)
    case RenderMembers(clsName, elems)                                    => renderMembers(clsName, elems)
    case EncodeEnum(clsName)                                              => encodeEnum(clsName)
    case DecodeEnum(clsName)                                              => decodeEnum(clsName)
    case RenderClass(clsName, tpe, elems)                                 => renderClass(clsName, tpe, elems)
    case RenderStaticDefns(clsName, members, accessors, encoder, decoder) => renderStaticDefns(clsName, members, accessors, encoder, decoder)
    case BuildAccessor(clsName, termName)                                 => buildAccessor(clsName, termName)
  }
}

object EnumProtocolTerms {
  implicit def enumProtocolTerms[L <: LA, F[_]](implicit I: InjectK[EnumProtocolTerm[L, ?], F]): EnumProtocolTerms[L, Free[F, ?]] =
    new EnumProtocolTerms[L, Free[F, ?]] {
      def MonadF                                                                 = Free.catsFreeMonadForFree
      def extractEnum(swagger: Schema[_]): Free[F, Either[String, List[String]]] = Free.inject[EnumProtocolTerm[L, ?], F](ExtractEnum[L](swagger))
      def renderMembers(clsName: String, elems: List[(String, L#TermName, L#TermSelect)]): Free[F, Option[L#ObjectDefinition]] =
        Free.inject[EnumProtocolTerm[L, ?], F](RenderMembers[L](clsName, elems))
      def encodeEnum(clsName: String): Free[F, Option[L#ValueDefinition]] = Free.inject[EnumProtocolTerm[L, ?], F](EncodeEnum[L](clsName))
      def decodeEnum(clsName: String): Free[F, Option[L#ValueDefinition]] = Free.inject[EnumProtocolTerm[L, ?], F](DecodeEnum[L](clsName))
      def renderClass(clsName: String, tpe: L#Type, elems: List[(String, L#TermName, L#TermSelect)]): Free[F, L#ClassDefinition] =
        Free.inject[EnumProtocolTerm[L, ?], F](RenderClass[L](clsName, tpe, elems))
      def renderStaticDefns(
          clsName: String,
          members: Option[L#ObjectDefinition],
          accessors: List[L#TermName],
          encoder: Option[L#ValueDefinition],
          decoder: Option[L#ValueDefinition]
      ): Free[F, StaticDefns[L]]                                                  = Free.inject[EnumProtocolTerm[L, ?], F](RenderStaticDefns[L](clsName, members, accessors, encoder, decoder))
      def buildAccessor(clsName: String, termName: String): Free[F, L#TermSelect] = Free.inject[EnumProtocolTerm[L, ?], F](BuildAccessor[L](clsName, termName))
    }
}
