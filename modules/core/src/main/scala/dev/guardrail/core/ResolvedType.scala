package dev.guardrail.core

import cats.{ FlatMap, Monad }
import cats.syntax.all._

import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, OpenAPITerms }

sealed trait ReifiedRawType
object ReifiedRawType {
  def of(rawType: Option[String], rawFormat: Option[String]): ReifiedRawType = LiteralRawType(rawType, rawFormat)
  def ofVector(rawType: ReifiedRawType): ReifiedRawType                      = VectorRawType(rawType)
  def ofMap(rawType: ReifiedRawType): ReifiedRawType                         = MapRawType(rawType)
  val unsafeEmpty: ReifiedRawType                                            = LiteralRawType(None, None)
}
case class LiteralRawType(rawType: Option[String], rawFormat: Option[String]) extends ReifiedRawType
case class VectorRawType(items: ReifiedRawType)                               extends ReifiedRawType
case class MapRawType(items: ReifiedRawType)                                  extends ReifiedRawType

case class Resolved[L <: LA](tpe: L#Type, classDep: Option[L#TermName], defaultValue: Option[L#Term], rawType: ReifiedRawType)
sealed trait LazyResolvedType[L <: LA]                                         extends { def value: String }
case class Deferred[L <: LA](value: String)                                    extends LazyResolvedType[L]
case class DeferredArray[L <: LA](value: String, containerTpe: Option[L#Type]) extends LazyResolvedType[L]
case class DeferredMap[L <: LA](value: String, containerTpe: Option[L#Type])   extends LazyResolvedType[L]

object LazyResolvedType {
  def map[L <: LA, F[_]: Monad, Out](deferred: LazyResolvedType[L])(func: String => (L#Type => F[L#Type]) => (ReifiedRawType => ReifiedRawType) => Out)(implicit
      Cl: CollectionsLibTerms[L, F]
  ) =
    deferred match {
      case Deferred(name) =>
        func(name)(_.pure[F])(identity _)
      case DeferredArray(name, containerTpe) =>
        func(name)(Cl.liftVectorType(_, containerTpe))(VectorRawType(_))
      case DeferredMap(name, containerTpe) =>
        func(name)(Cl.liftMapType(_, containerTpe))(MapRawType(_))
    }

  def lift[L <: LA, F[_]: Monad](deferred: LazyResolvedType[L], protocolElems: List[StrictProtocolElems[L]])(implicit
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: OpenAPITerms[L, F]
  ): F[Resolved[L]] =
    map[L, F, F[Resolved[L]]](deferred)(name => liftType => liftRawType => branch(name, protocolElems)(liftType, liftRawType))

  private[this] def branch[L <: LA, F[_]: Monad](name: String, protocolElems: List[StrictProtocolElems[L]])(
      liftType: L#Type => F[L#Type],
      liftRawType: ReifiedRawType => ReifiedRawType
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: OpenAPITerms[L, F]): F[Resolved[L]] = {
    import Sc._
    import Sw._
    for {
      formattedName <- formatTypeName(name)
      resolved <- resolveType(formattedName, protocolElems)
        .flatMap {
          case RandomType(name, tpe) =>
            liftType(tpe).map(Resolved[L](_, None, None, liftRawType(ReifiedRawType.unsafeEmpty)))
          case ClassDefinition(name, _, fullType, cls, _, _) =>
            liftType(fullType).map(Resolved[L](_, None, None, liftRawType(ReifiedRawType.unsafeEmpty)))
          case EnumDefinition(name, _, fullType, _, cls, _) =>
            liftType(fullType).map(Resolved[L](_, None, None, liftRawType(ReifiedRawType.of(Some("string"), None))))
          case ADT(_, _, fullType, _, _) =>
            liftType(fullType).map(Resolved[L](_, None, None, liftRawType(ReifiedRawType.unsafeEmpty)))
        }
    } yield resolved
  }
}

object ResolvedType {
  def resolveReferences[L <: LA, F[_]: Monad](
      values: List[(String, Either[LazyResolvedType[L], Resolved[L]])]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: OpenAPITerms[L, F]): F[List[(String, Resolved[L])]] =
    Sw.log.function("resolveReferences") {
      import Sw._
      val (lazyTypes, resolvedTypes) = values.partitionEither {
        case (clsName, Right(x: Resolved[L]))        => Right((clsName, x))
        case (clsName, Left(x: LazyResolvedType[L])) => Left((clsName, x))
      }

      def lookupTypeName(tpeName: String, resolvedTypes: List[(String, Resolved[L])])(
          f: L#Type => F[L#Type]
      ): F[Option[Resolved[L]]] =
        resolvedTypes
          .find(_._1 == tpeName)
          .map(_._2)
          .traverse(x => f(x.tpe).map(tpe => x.copy(tpe = tpe)))

      type Continue = (List[(String, LazyResolvedType[L])], List[(String, Resolved[L])])
      type Stop     = List[(String, Resolved[L])]
      log.debug(s"resolve ${values.length} references") >> FlatMap[F]
        .tailRecM[Continue, Stop](
          (lazyTypes, resolvedTypes)
        ) { case (lazyTypes, resolvedTypes) =>
          for {
            (newLazyTypes, additionalResolvedTypes) <- lazyTypes
              .partitionEitherM { case x @ (clsName, deferred) =>
                LazyResolvedType
                  .map[L, F, F[Option[Resolved[L]]]](deferred)(name => liftType => _ => lookupTypeName(name, resolvedTypes)(liftType))
                  .map(resolved => Either.fromOption(resolved, x).map((clsName, _)))
              }
            newResolvedTypes = resolvedTypes ++ additionalResolvedTypes
          } yield
            if (lazyTypes.isEmpty || lazyTypes.forall(newLazyTypes.contains))
              Right(newResolvedTypes)
            else
              Left((newLazyTypes, newResolvedTypes))
        }
    }

  def resolve[L <: LA, F[_]: Monad](
      value: Either[LazyResolvedType[L], Resolved[L]],
      protocolElems: List[StrictProtocolElems[L]]
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: OpenAPITerms[L, F]): F[Resolved[L]] =
    for {
      _ <- Sw.log.debug(s"value: ${value} in ${protocolElems.length} protocol elements")
      res <- value
        .bimap(LazyResolvedType.lift[L, F](_, protocolElems), _.pure[F])
        .merge
    } yield res
}
