package com.twilio.guardrail.protocol.terms.protocol

import com.twilio.guardrail.languages.LA
import cats.InjectK

class AliasProtocolTerms[L <: LA, F[_]](implicit I: InjectK[AliasProtocolTerm[L, ?], F]) {}
object AliasProtocolTerms {
  implicit def aliasProtocolTerm[L <: LA, F[_]](implicit I: InjectK[AliasProtocolTerm[L, ?], F]): AliasProtocolTerms[L, F] =
    new AliasProtocolTerms[L, F]
}
