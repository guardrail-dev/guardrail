package com.twilio.swagger.codegen
package terms.protocol

import _root_.io.swagger.models.Model
import cats.free.{Free, Inject}
import scala.meta._

class ProtocolSupportTerms[F[_]](implicit I: Inject[ProtocolSupportTerm, F]) {
  def extractConcreteTypes(models: List[(String, Model)]): Free[F, List[PropMeta]] =
    Free.inject[ProtocolSupportTerm, F](ExtractConcreteTypes(models))
  def protocolImports(): Free[F, List[Import]] =
    Free.inject[ProtocolSupportTerm, F](ProtocolImports())
  def packageObjectImports(): Free[F, List[Import]] =
    Free.inject[ProtocolSupportTerm, F](PackageObjectImports())
  def packageObjectContents(): Free[F, List[Stat]] =
    Free.inject[ProtocolSupportTerm, F](PackageObjectContents())
}
object ProtocolSupportTerms {
  implicit def protocolSupportTerms[F[_]](implicit I: Inject[ProtocolSupportTerm, F]): ProtocolSupportTerms[F] = new ProtocolSupportTerms[F]
}
