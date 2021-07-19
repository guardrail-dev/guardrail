package dev.guardrail
package generators

import dev.guardrail.languages.LA
import dev.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import dev.guardrail.protocol.terms.client.ClientTerms
import dev.guardrail.protocol.terms.server.ServerTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }
import dev.guardrail.terms.framework.FrameworkTerms

trait Framework[L <: LA, F[_]] {
  implicit def ArrayProtocolInterp: ArrayProtocolTerms[L, F]
  implicit def ClientInterp: ClientTerms[L, F]
  implicit def EnumProtocolInterp: EnumProtocolTerms[L, F]
  implicit def FrameworkInterp: FrameworkTerms[L, F]
  implicit def ModelProtocolInterp: ModelProtocolTerms[L, F]
  implicit def PolyProtocolInterp: PolyProtocolTerms[L, F]
  implicit def ProtocolSupportInterp: ProtocolSupportTerms[L, F]
  implicit def ServerInterp: ServerTerms[L, F]
  implicit def SwaggerInterp: SwaggerTerms[L, F]
  implicit def LanguageInterp: LanguageTerms[L, F]
  implicit def CollectionsLibInterp: CollectionsLibTerms[L, F]
}
