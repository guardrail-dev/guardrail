package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import cats.Monad
import com.twilio.guardrail.StaticDefns
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.CollectionsLibTerms

abstract class EnumProtocolTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) {
  def MonadF: Monad[F]
  def extractEnum(swagger: Tracker[Schema[_]]): F[Either[String, List[String]]]
  def renderMembers(clsName: String, elems: List[(String, L#TermName, L#TermSelect)]): F[Option[L#ObjectDefinition]]
  def encodeEnum(clsName: String): F[Option[L#Definition]]
  def decodeEnum(clsName: String): F[Option[L#Definition]]
  def renderClass(clsName: String, tpe: L#Type, elems: List[(String, L#TermName, L#TermSelect)]): F[L#ClassDefinition]
  def renderStaticDefns(
      clsName: String,
      members: Option[L#ObjectDefinition],
      accessors: List[L#TermName],
      encoder: Option[L#Definition],
      decoder: Option[L#Definition]
  ): F[StaticDefns[L]]
  def buildAccessor(clsName: String, termName: String): F[L#TermSelect]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newExtractEnum: Tracker[Schema[_]] => F[Either[String, List[String]]] = extractEnum _,
      newRenderMembers: (String, List[(String, L#TermName, L#TermSelect)]) => F[Option[L#ObjectDefinition]] = renderMembers _,
      newEncodeEnum: String => F[Option[L#Definition]] = encodeEnum _,
      newDecodeEnum: String => F[Option[L#Definition]] = decodeEnum _,
      newRenderClass: (String, L#Type, List[(String, L#TermName, L#TermSelect)]) => F[L#ClassDefinition] = renderClass _,
      newRenderStaticDefns: (String, Option[L#ObjectDefinition], List[L#TermName], Option[L#Definition], Option[L#Definition]) => F[StaticDefns[L]] =
        renderStaticDefns _,
      newBuildAccessor: (String, String) => F[L#TermSelect] = buildAccessor _
  ) = new EnumProtocolTerms[L, F] {
    def MonadF                                                                                     = newMonadF
    def extractEnum(swagger: Tracker[Schema[_]])                                                   = newExtractEnum(swagger)
    def renderMembers(clsName: String, elems: List[(String, L#TermName, L#TermSelect)])            = newRenderMembers(clsName, elems)
    def encodeEnum(clsName: String): F[Option[L#Definition]]                                       = newEncodeEnum(clsName)
    def decodeEnum(clsName: String): F[Option[L#Definition]]                                       = newDecodeEnum(clsName)
    def renderClass(clsName: String, tpe: L#Type, elems: List[(String, L#TermName, L#TermSelect)]) = newRenderClass(clsName, tpe, elems)
    def renderStaticDefns(
        clsName: String,
        members: Option[L#ObjectDefinition],
        accessors: List[L#TermName],
        encoder: Option[L#Definition],
        decoder: Option[L#Definition]
    ): F[StaticDefns[L]]                                 = newRenderStaticDefns(clsName, members, accessors, encoder, decoder)
    def buildAccessor(clsName: String, termName: String) = newBuildAccessor(clsName, termName)
  }
}
