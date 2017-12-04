package com.twilio.swagger.codegen
package terms.protocol

import cats.free.{Free, Inject}
import scala.meta._

class AliasProtocolTerms[F[_]](implicit I: Inject[AliasProtocolTerm, F]) {
}
object AliasProtocolTerms {
  implicit def aliasProtocolTerm[F[_]](implicit I: Inject[AliasProtocolTerm, F]): AliasProtocolTerms[F] = new AliasProtocolTerms[F]
}
