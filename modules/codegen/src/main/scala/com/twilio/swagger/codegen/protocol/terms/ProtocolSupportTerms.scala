package com.twilio.swagger.codegen
package terms.protocol

import cats.free.{Free, Inject}
import scala.collection.immutable.Seq
import scala.meta._

class ProtocolSupportTerms[F[_]](implicit I: Inject[ProtocolSupportTerm, F]) {
  def protocolImports(): Free[F, Seq[Import]] =
    Free.inject[ProtocolSupportTerm, F](ProtocolImports())
  def packageObjectImports(): Free[F, Seq[Import]] =
    Free.inject[ProtocolSupportTerm, F](PackageObjectImports())
  def packageObjectContents(): Free[F, Seq[Stat]] =
    Free.inject[ProtocolSupportTerm, F](PackageObjectContents())
}
object ProtocolSupportTerms {
  implicit def protocolSupportTerms[F[_]](implicit I: Inject[ProtocolSupportTerm, F]): ProtocolSupportTerms[F] = new ProtocolSupportTerms[F]
}
