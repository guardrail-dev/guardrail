package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.StaticDefns
import com.twilio.guardrail.languages.LA

class EnumProtocolTerms[L <: LA, F[_]](implicit I: InjectK[EnumProtocolTerm[L, ?], F]) {
  def extractEnum(swagger: Schema[_]): Free[F, Either[String, List[String]]] =
    Free.inject[EnumProtocolTerm[L, ?], F](ExtractEnum[L](swagger))
  def renderMembers(clsName: String, elems: List[(String, L#TermName, L#TermSelect)]): Free[F, Option[L#ObjectDefinition]] =
    Free.inject[EnumProtocolTerm[L, ?], F](RenderMembers[L](clsName, elems))
  def encodeEnum(clsName: String): Free[F, Option[L#ValueDefinition]] =
    Free.inject[EnumProtocolTerm[L, ?], F](EncodeEnum[L](clsName))
  def decodeEnum(clsName: String): Free[F, Option[L#ValueDefinition]] =
    Free.inject[EnumProtocolTerm[L, ?], F](DecodeEnum[L](clsName))
  def renderClass(clsName: String, tpe: L#Type, elems: List[(String, L#TermName, L#TermSelect)]): Free[F, L#ClassDefinition] =
    Free.inject[EnumProtocolTerm[L, ?], F](RenderClass[L](clsName, tpe, elems))
  def renderStaticDefns(
      clsName: String,
      members: Option[L#ObjectDefinition],
      accessors: List[L#TermName],
      encoder: Option[L#ValueDefinition],
      decoder: Option[L#ValueDefinition]
  ): Free[F, StaticDefns[L]] =
    Free.inject[EnumProtocolTerm[L, ?], F](RenderStaticDefns[L](clsName, members, accessors, encoder, decoder))
  def buildAccessor(clsName: String, termName: String): Free[F, L#TermSelect] =
    Free.inject[EnumProtocolTerm[L, ?], F](BuildAccessor[L](clsName, termName))
}

object EnumProtocolTerms {
  implicit def enumProtocolTerms[L <: LA, F[_]](implicit I: InjectK[EnumProtocolTerm[L, ?], F]): EnumProtocolTerms[L, F] =
    new EnumProtocolTerms[L, F]
}
