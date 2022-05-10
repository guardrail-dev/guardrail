package dev.guardrail.terms

import io.swagger.v3.oas.models.security.{ OAuthFlows, SecurityScheme => SwSecurityScheme }
import java.net.URI

import dev.guardrail.languages.LA

sealed trait SecurityScheme[L <: LA] {
  def tpe: Option[L#Type]
}
case class ApiKeySecurityScheme[L <: LA](name: String, in: SwSecurityScheme.In, tpe: Option[L#Type]) extends SecurityScheme[L]
case class HttpSecurityScheme[L <: LA](authScheme: String, tpe: Option[L#Type])                      extends SecurityScheme[L]
case class MutualTLSSecurityScheme[L <: LA](tpe: Option[L#Type])                                     extends SecurityScheme[L]
case class OAuth2SecurityScheme[L <: LA](flows: OAuthFlows, tpe: Option[L#Type])                     extends SecurityScheme[L]
case class OpenIdConnectSecurityScheme[L <: LA](url: URI, tpe: Option[L#Type])                       extends SecurityScheme[L]
