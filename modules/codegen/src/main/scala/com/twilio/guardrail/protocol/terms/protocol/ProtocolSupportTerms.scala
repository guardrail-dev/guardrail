package com.twilio.guardrail.protocol.terms.protocol

import io.swagger.v3.oas.models.media.Schema
import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.{ ProtocolElems, StrictProtocolElems }

class ProtocolSupportTerms[L <: LA, F[_]](implicit I: InjectK[ProtocolSupportTerm[L, ?], F]) {
  def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]): Free[F, List[PropMeta[L]]] =
    Free.inject[ProtocolSupportTerm[L, ?], F](ExtractConcreteTypes(models))
  def protocolImports(): Free[F, List[L#Import]] =
    Free.inject[ProtocolSupportTerm[L, ?], F](ProtocolImports())
  def packageObjectImports(): Free[F, List[L#Import]] =
    Free.inject[ProtocolSupportTerm[L, ?], F](PackageObjectImports())
  def packageObjectContents(): Free[F, List[L#ValueDefinition]] =
    Free.inject[ProtocolSupportTerm[L, ?], F](PackageObjectContents())
}
object ProtocolSupportTerms {
  implicit def protocolSupportTerms[L <: LA, F[_]](implicit I: InjectK[ProtocolSupportTerm[L, ?], F]): ProtocolSupportTerms[L, F] =
    new ProtocolSupportTerms[L, F]
}
