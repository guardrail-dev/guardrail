package dev.guardrail.generators

import dev.guardrail.languages.LA
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, OpenAPITerms, ProtocolTerms }
import dev.guardrail.terms.framework.FrameworkTerms

trait Framework[L <: LA, F[_]] {
  implicit def ClientInterp: ClientTerms[L, F]
  implicit def FrameworkInterp: FrameworkTerms[L, F]
  implicit def ProtocolInterp: ProtocolTerms[L, F]
  implicit def ServerInterp: ServerTerms[L, F]
  implicit def SwaggerInterp: OpenAPITerms[L, F]
  implicit def LanguageInterp: LanguageTerms[L, F]
  implicit def CollectionsLibInterp: CollectionsLibTerms[L, F]

  def copy(
      clientInterp: ClientTerms[L, F] = this.ClientInterp,
      frameworkInterp: FrameworkTerms[L, F] = this.FrameworkInterp,
      protocolInterp: ProtocolTerms[L, F] = this.ProtocolInterp,
      serverInterp: ServerTerms[L, F] = this.ServerInterp,
      swaggerInterp: OpenAPITerms[L, F] = this.SwaggerInterp,
      languageInterp: LanguageTerms[L, F] = this.LanguageInterp,
      collectionsLibInterp: CollectionsLibTerms[L, F] = this.CollectionsLibInterp
  ): Framework[L, F] = {
    val newClientInterp         = clientInterp
    val newFrameworkInterp      = frameworkInterp
    val newProtocolInterp       = protocolInterp
    val newServerInterp         = serverInterp
    val newSwaggerInterp        = swaggerInterp
    val newLanguageInterp       = languageInterp
    val newCollectionsLibInterp = collectionsLibInterp

    new Framework[L, F] {
      implicit def ClientInterp: ClientTerms[L, F]                          = newClientInterp
      implicit def FrameworkInterp: FrameworkTerms[L, F]                    = newFrameworkInterp
      implicit def ProtocolInterp: ProtocolTerms[L, F]                      = newProtocolInterp
      implicit def ServerInterp: ServerTerms[L, F]                          = newServerInterp
      implicit def SwaggerInterp: OpenAPITerms[L, F]                        = newSwaggerInterp
      implicit def LanguageInterp: LanguageTerms[L, F]                      = newLanguageInterp
      override implicit def CollectionsLibInterp: CollectionsLibTerms[L, F] = newCollectionsLibInterp
    }
  }
}
