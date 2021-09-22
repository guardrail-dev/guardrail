package dev.guardrail.core

import cats.{ FlatMap, Foldable }
import cats.syntax.all._

import dev.guardrail._
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }

sealed trait ResolvedType[L <: LA]
case class Resolved[L <: LA](tpe: L#Type, classDep: Option[L#TermName], defaultValue: Option[L#Term], rawType: Option[String], rawFormat: Option[String])
    extends ResolvedType[L]
sealed trait LazyResolvedType[L <: LA]                                         extends ResolvedType[L]
case class Deferred[L <: LA](value: String)                                    extends LazyResolvedType[L]
case class DeferredArray[L <: LA](value: String, containerTpe: Option[L#Type]) extends LazyResolvedType[L]
case class DeferredMap[L <: LA](value: String, containerTpe: Option[L#Type])   extends LazyResolvedType[L]

object ResolvedType {
  def resolveReferences[L <: LA, F[_]](
      values: List[(String, ResolvedType[L])]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F]): F[List[(String, Resolved[L])]] =
    Sw.log.function("resolveReferences") {
      import Cl._
      import Sw._
      val (lazyTypes, resolvedTypes) = Foldable[List].partitionEither(values) {
        case (clsName, x: Resolved[L])         => Right((clsName, x))
        case (clsName, x: LazyResolvedType[L]) => Left((clsName, x))
      }

      def lookupTypeName(clsName: String, tpeName: String, resolvedTypes: List[(String, Resolved[L])])(
          f: L#Type => F[L#Type]
      ): F[Option[(String, Resolved[L])]] =
        resolvedTypes
          .find(_._1 == tpeName)
          .map(_._2)
          .traverse(x => f(x.tpe).map(tpe => (clsName, x.copy(tpe = tpe))))

      type Continue = (List[(String, LazyResolvedType[L])], List[(String, Resolved[L])])
      type Stop     = List[(String, Resolved[L])]
      log.debug(s"resolve ${values.length} references") >> FlatMap[F]
        .tailRecM[Continue, Stop](
          (lazyTypes, resolvedTypes)
        ) {
          case (lazyTypes, resolvedTypes) =>
            if (lazyTypes.isEmpty) {
              (Right(resolvedTypes): Either[Continue, Stop]).pure[F]
            } else {
              lazyTypes
                .partitionEitherM({
                  case x @ (clsName, Deferred(tpeName)) =>
                    lookupTypeName(clsName, tpeName, resolvedTypes)(_.pure[F]).map(Either.fromOption(_, x))
                  case x @ (clsName, DeferredArray(tpeName, containerTpe)) =>
                    lookupTypeName(clsName, tpeName, resolvedTypes)(liftVectorType(_, containerTpe)).map(Either.fromOption(_, x))
                  case x @ (clsName, DeferredMap(tpeName, containerTpe)) =>
                    lookupTypeName(clsName, tpeName, resolvedTypes)(liftMapType(_, containerTpe)).map(Either.fromOption(_, x))
                })
                .map({
                  case (newLazyTypes, newResolvedTypes) =>
                    Left((newLazyTypes, resolvedTypes ++ newResolvedTypes))
                })
            }
        }
    }

  def resolve[L <: LA, F[_]](
      value: ResolvedType[L],
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F]): F[Resolved[L]] = {
    import Sc._
    import Cl._
    import Sw._
    log.debug(s"value: ${value} in ${protocolElems.length} protocol elements") >> (value match {
      case x @ Resolved(_, _, _, _, _) => x.pure[F]
      case Deferred(name) =>
        for {
          formattedName <- formatTypeName(name)
          resolved <- resolveType(formattedName, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                Resolved[L](tpe, None, None, None, None).pure[F]
              case ClassDefinition(name, _, fullType, cls, _, _) =>
                Resolved[L](fullType, None, None, None, None).pure[F]
              case EnumDefinition(name, _, fullType, _, cls, _) =>
                Resolved[L](fullType, None, None, Some("string"), None).pure[F]
              case ADT(_, _, fullType, _, _) =>
                Resolved[L](fullType, None, None, None, None).pure[F]
            }
        } yield resolved
      case DeferredArray(name, containerTpe) =>
        for {
          formattedName <- formatTypeName(name)
          resolved <- resolveType(formattedName, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                liftVectorType(tpe, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ClassDefinition(name, _, fullType, cls, _, _) =>
                liftVectorType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case EnumDefinition(name, _, fullType, _, cls, _) =>
                liftVectorType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ADT(_, _, fullType, _, _) =>
                liftVectorType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
            }
        } yield resolved
      case DeferredMap(name, containerTpe) =>
        for {
          formattedName <- formatTypeName(name)
          resolved <- resolveType(formattedName, protocolElems)
            .flatMap {
              case RandomType(name, tpe) =>
                liftMapType(tpe, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ClassDefinition(_, _, fullType, _, _, _) =>
                liftMapType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case EnumDefinition(_, _, fullType, _, _, _) =>
                liftMapType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
              case ADT(_, _, fullType, _, _) =>
                liftMapType(fullType, containerTpe).map(Resolved[L](_, None, None, None, None))
            }
        } yield resolved
    })
  }
}
