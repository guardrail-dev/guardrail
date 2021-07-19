package dev.guardrail.protocol.terms.protocol

import cats.Monad
import dev.guardrail.StaticDefns
import dev.guardrail.languages.LA
import dev.guardrail.terms.{ CollectionsLibTerms, RenderedEnum }

abstract class EnumProtocolTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) {
  def MonadF: Monad[F]
  def renderMembers(clsName: String, elems: RenderedEnum[L]): F[Option[L#ObjectDefinition]]
  def encodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]]
  def decodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]]
  def renderClass(clsName: String, tpe: L#Type, elems: RenderedEnum[L]): F[L#ClassDefinition]
  def renderStaticDefns(
      clsName: String,
      tpe: L#Type,
      members: Option[L#ObjectDefinition],
      accessors: List[L#TermName],
      encoder: Option[L#Definition],
      decoder: Option[L#Definition]
  ): F[StaticDefns[L]]
  def buildAccessor(clsName: String, termName: String): F[L#TermSelect]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newRenderMembers: (String, RenderedEnum[L]) => F[Option[L#ObjectDefinition]] = renderMembers _,
      newEncodeEnum: (String, L#Type) => F[Option[L#Definition]] = encodeEnum _,
      newDecodeEnum: (String, L#Type) => F[Option[L#Definition]] = decodeEnum _,
      newRenderClass: (String, L#Type, RenderedEnum[L]) => F[L#ClassDefinition] = renderClass _,
      newRenderStaticDefns: (String, L#Type, Option[L#ObjectDefinition], List[L#TermName], Option[L#Definition], Option[L#Definition]) => F[StaticDefns[L]] =
        renderStaticDefns _,
      newBuildAccessor: (String, String) => F[L#TermSelect] = buildAccessor _
  ) = new EnumProtocolTerms[L, F] {
    def MonadF                                                            = newMonadF
    def renderMembers(clsName: String, elems: RenderedEnum[L])            = newRenderMembers(clsName, elems)
    def encodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]] = newEncodeEnum(clsName, tpe)
    def decodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]] = newDecodeEnum(clsName, tpe)
    def renderClass(clsName: String, tpe: L#Type, elems: RenderedEnum[L]) = newRenderClass(clsName, tpe, elems)
    def renderStaticDefns(
        clsName: String,
        tpe: L#Type,
        members: Option[L#ObjectDefinition],
        accessors: List[L#TermName],
        encoder: Option[L#Definition],
        decoder: Option[L#Definition]
    ): F[StaticDefns[L]]                                 = newRenderStaticDefns(clsName, tpe, members, accessors, encoder, decoder)
    def buildAccessor(clsName: String, termName: String) = newBuildAccessor(clsName, termName)
  }
}
