package dev.guardrail

import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.security.{ SecurityScheme => SwSecurityScheme }
import cats.syntax.all._
import dev.guardrail.core.{ ReifiedRawType, Tracker }
import dev.guardrail.core.implicits._
import dev.guardrail.core.resolvers.ModelResolver
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SecurityScheme, SwaggerTerms }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.core.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, VendorExtension }
import dev.guardrail.core.extract.VendorExtension.VendorExtensible._
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.PropMeta

object SwaggerUtil {
  def customTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit Cl: CollectionsLibTerms[L, F]): F[Option[String]] = {
    import Cl._
    for {
      prefixes <- vendorPrefixes()
    } yield CustomTypeName(v, prefixes)
  }

  def customArrayTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit Cl: CollectionsLibTerms[L, F]): F[Option[String]] = {
    import Cl._
    for {
      prefixes <- vendorPrefixes()
    } yield CustomArrayTypeName(v, prefixes)
  }

  def customMapTypeName[L <: LA, F[_], A: VendorExtension.VendorExtensible](v: A)(implicit Cl: CollectionsLibTerms[L, F]): F[Option[String]] = {
    import Cl._
    for {
      prefixes <- vendorPrefixes()
    } yield CustomMapTypeName(v, prefixes)
  }

  def isFile(typeName: String, format: Option[String]): Boolean =
    (typeName, format) match {
      case ("string", Some("binary")) => true
      case ("file", _)                => true
      case ("binary", _)              => true
      case _                          => false
    }

  def extractConcreteTypes[L <: LA, F[_]](
      definitions: List[(String, Tracker[Schema[_]])],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): F[List[PropMeta[L]]] = {
    import Sc._
    for {
      entries <- definitions.traverse[F, (String, core.ResolvedType[L])] { case (clsName, schema) =>
        schema
          .refine { case impl: Schema[_] if Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined => impl }(impl =>
            for {
              formattedClsName <- formatTypeName(clsName)
              typeName         <- pureTypeName(formattedClsName)
              widenedTypeName  <- widenTypeName(typeName)
            } yield (clsName, core.Resolved[L](widenedTypeName, None, None, ReifiedRawType.unsafeEmpty): core.ResolvedType[L])
          )
          .orRefine { case comp: ComposedSchema => comp }(comp =>
            for {
              formattedClsName <- formatTypeName(clsName)
              typeName         <- pureTypeName(formattedClsName)
              widenedTypeName  <- widenTypeName(typeName)
              parentSimpleRef = comp
                .downField("allOf", _.getAllOf)
                .indexedDistribute
                .headOption
                .flatMap(_.downField("$ref", _.get$ref).indexedDistribute)
                .map(_.unwrapTracker.split("/").last)
              parentTerm <- parentSimpleRef.traverse(n => pureTermName(n))
              resolvedType = core.Resolved[L](widenedTypeName, parentTerm, None, ReifiedRawType.unsafeEmpty): core.ResolvedType[L]
            } yield (clsName, resolvedType)
          )
          .getOrElse(
            for {
              resolved <- ModelResolver.modelMetaType[L, F](schema, components)
            } yield (clsName, resolved)
          )
      }
      result <- core.ResolvedType.resolveReferences[L, F](entries)
    } yield result.map { case (clsName, core.Resolved(tpe, _, _, _)) =>
      PropMeta[L](clsName, tpe) // TODO: We're losing ReifiedRawType here. Perhaps maintain through PropMeta?
    }
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
