package com.twilio.swagger.codegen
package terms.protocol

import cats.free.{Free, Inject}
import scala.meta._

class ProtocolSupportTerms[F[_]](implicit I: Inject[ProtocolSupportTerm, F]) {
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
