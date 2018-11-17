package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.ModelImpl
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.languages.LA

class EnumProtocolTerms[L <: LA, F[_]](implicit I: InjectK[EnumProtocolTerm[L, ?], F]) {
  def extractEnum(swagger: ModelImpl): Free[F, Either[String, List[String]]] =
    Free.inject[EnumProtocolTerm[L, ?], F](ExtractEnum[L](swagger))
  def extractType(swagger: ModelImpl): Free[F, Either[String, L#Type]] =
    Free.inject[EnumProtocolTerm[L, ?], F](ExtractType[L](swagger))
  def renderMembers(clsName: String, elems: List[(String, L#TermName, L#Term)]): Free[F, L#ObjectDefinition] =
    Free.inject[EnumProtocolTerm[L, ?], F](RenderMembers[L](clsName, elems))
  def encodeEnum(clsName: String): Free[F, L#ValueDefinition] =
    Free.inject[EnumProtocolTerm[L, ?], F](EncodeEnum[L](clsName))
  def decodeEnum(clsName: String): Free[F, L#ValueDefinition] =
    Free.inject[EnumProtocolTerm[L, ?], F](DecodeEnum[L](clsName))
  def renderClass(clsName: String, tpe: L#Type): Free[F, L#ClassDefinition] =
    Free.inject[EnumProtocolTerm[L, ?], F](RenderClass[L](clsName, tpe))
  def renderCompanion(clsName: String,
                      members: L#ObjectDefinition,
                      accessors: List[L#ValueDefinition],
                      values: L#ValueDefinition,
                      encoder: L#ValueDefinition,
                      decoder: L#ValueDefinition): Free[F, L#ObjectDefinition] =
    Free.inject[EnumProtocolTerm[L, ?], F](RenderCompanion[L](clsName, members, accessors, values, encoder, decoder))
}

object EnumProtocolTerms {
  implicit def enumProtocolTerms[L <: LA, F[_]](implicit I: InjectK[EnumProtocolTerm[L, ?], F]): EnumProtocolTerms[L, F] =
    new EnumProtocolTerms[L, F]
}
