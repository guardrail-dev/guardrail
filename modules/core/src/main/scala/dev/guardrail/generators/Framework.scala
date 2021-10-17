package dev.guardrail.generators

import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
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

  def copy(
      arrayProtocolInterp: ArrayProtocolTerms[L, F] = this.ArrayProtocolInterp,
      clientInterp: ClientTerms[L, F] = this.ClientInterp,
      enumProtocolInterp: EnumProtocolTerms[L, F] = this.EnumProtocolInterp,
      frameworkInterp: FrameworkTerms[L, F] = this.FrameworkInterp,
      modelProtocolInterp: ModelProtocolTerms[L, F] = this.ModelProtocolInterp,
      polyProtocolInterp: PolyProtocolTerms[L, F] = this.PolyProtocolInterp,
      protocolSupportInterp: ProtocolSupportTerms[L, F] = this.ProtocolSupportInterp,
      serverInterp: ServerTerms[L, F] = this.ServerInterp,
      swaggerInterp: SwaggerTerms[L, F] = this.SwaggerInterp,
      languageInterp: LanguageTerms[L, F] = this.LanguageInterp,
      collectionsLibInterp: CollectionsLibTerms[L, F] = this.CollectionsLibInterp
  ): Framework[L, F] = {
    val newArrayProtocolInterp   = arrayProtocolInterp
    val newClientInterp          = clientInterp
    val newEnumProtocolInterp    = enumProtocolInterp
    val newFrameworkInterp       = frameworkInterp
    val newModelProtocolInterp   = modelProtocolInterp
    val newPolyProtocolInterp    = polyProtocolInterp
    val newProtocolSupportInterp = protocolSupportInterp
    val newServerInterp          = serverInterp
    val newSwaggerInterp         = swaggerInterp
    val newLanguageInterp        = languageInterp
    val newCollectionsLibInterp  = collectionsLibInterp

    new Framework[L, F] {
      implicit def ArrayProtocolInterp   = newArrayProtocolInterp
      implicit def ClientInterp          = newClientInterp
      implicit def EnumProtocolInterp    = newEnumProtocolInterp
      implicit def FrameworkInterp       = newFrameworkInterp
      implicit def ModelProtocolInterp   = newModelProtocolInterp
      implicit def PolyProtocolInterp    = newPolyProtocolInterp
      implicit def ProtocolSupportInterp = newProtocolSupportInterp
      implicit def ServerInterp          = newServerInterp
      implicit def SwaggerInterp         = newSwaggerInterp
      implicit def LanguageInterp        = newLanguageInterp
      implicit def CollectionsLibInterp  = newCollectionsLibInterp
    }
  }
}
