package dev.guardrail

import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import cats.syntax.all._
import dev.guardrail.core.Tracker
import dev.guardrail.core.implicits._
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SecurityScheme, SwaggerTerms }
import dev.guardrail.core.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, VendorExtension }
import dev.guardrail.core.extract.VendorExtension.VendorExtensible._
import dev.guardrail.languages.LA

object SwaggerUtil {
  def isFile(typeName: String, format: Option[String]): Boolean =
    (typeName, format) match {
      case ("string", Some("binary")) => true
      case ("file", _)                => true
      case ("binary", _)              => true
      case _                          => false
    }

  def extractSecuritySchemes[L <: LA, F[_]](
      spec: OpenAPI,
      prefixes: List[String]
  )(implicit Sw: SwaggerTerms[L, F], Sc: LanguageTerms[L, F]): F[Map[String, SecurityScheme[L]]] = {
    import Sw._
    import Sc._

    Tracker(spec)
      .downField("components", _.getComponents)
      .flatDownField("securitySchemes", _.getSecuritySchemes)
      .indexedDistribute
      .value
      .flatTraverse { case (schemeName, scheme) =>
        val typeName = CustomTypeName(scheme, prefixes)
        for {
          tpe <- typeName.fold(Option.empty[L#Type].pure[F])(x => parseType(Tracker.cloneHistory(scheme, x)))
          parsedScheme <- scheme.downField("type", _.getType).unwrapTracker.traverse {
            case SwSecurityScheme.Type.APIKEY        => extractApiKeySecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.HTTP          => extractHttpSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.OPENIDCONNECT => extractOpenIdConnectSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.OAUTH2        => extractOAuth2SecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
            case SwSecurityScheme.Type.MUTUALTLS     => extractMutualTLSSecurityScheme(schemeName, scheme, tpe).widen[SecurityScheme[L]]
          }
        } yield parsedScheme.toList.map(scheme => schemeName -> scheme)
      }
      .map(_.toMap)
  }
}
