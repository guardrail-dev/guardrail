package dev.guardrail.terms.protocol

import cats.Monad
import cats.syntax.all._
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media._

import dev.guardrail.languages.LA
import dev.guardrail.core.{ ReifiedRawType, Resolved, ResolvedType, Tracker }
import dev.guardrail.core.resolvers.ModelResolver
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }
import dev.guardrail.terms.framework.FrameworkTerms

case class PropMeta[L <: LA](clsName: String, tpe: L#Type)

object PropMeta {
  def extractConcreteTypes[L <: LA, F[_]: Monad](
      definitions: List[(String, Tracker[Schema[_]])],
      components: Tracker[Option[Components]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], F: FrameworkTerms[L, F]): F[List[PropMeta[L]]] = {
    import Sc._
    for {
      entries <- definitions.traverse[F, (String, ResolvedType[L])] { case (clsName, schema) =>
        schema
          .refine { case impl: Schema[_] if Option(impl.getProperties()).isDefined || Option(impl.getEnum()).isDefined => impl }(impl =>
            for {
              formattedClsName <- formatTypeName(clsName)
              typeName         <- pureTypeName(formattedClsName)
              widenedTypeName  <- widenTypeName(typeName)
            } yield (clsName, Resolved[L](widenedTypeName, None, None, ReifiedRawType.unsafeEmpty): ResolvedType[L])
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
              resolvedType = Resolved[L](widenedTypeName, parentTerm, None, ReifiedRawType.unsafeEmpty): ResolvedType[L]
            } yield (clsName, resolvedType)
          )
          .getOrElse(
            for {
              resolved <- ModelResolver.modelMetaType[L, F](schema, components)
            } yield (clsName, resolved)
          )
      }
      result <- ResolvedType.resolveReferences[L, F](entries)
    } yield result.map { case (clsName, Resolved(tpe, _, _, _)) =>
      PropMeta[L](clsName, tpe) // TODO: We're losing ReifiedRawType here. Perhaps maintain through PropMeta?
    }
  }
}
