package dev.guardrail

import cats.FlatMap
import cats.syntax.all._
import dev.guardrail.languages.LA
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, RenderedEnum, SwaggerTerms }
import dev.guardrail.protocol.terms.protocol.ProtocolSupportTerms

case class StaticDefns[L <: LA](className: String, extraImports: List[L#Import], definitions: List[L#Definition])

sealed trait ProtocolElems[L <: LA] { def name: String }

sealed trait LazyProtocolElems[L <: LA]                                    extends ProtocolElems[L]
case class Deferred[L <: LA](name: String)                                 extends LazyProtocolElems[L]
case class DeferredArray[L <: LA](name: String, customTpe: Option[L#Type]) extends LazyProtocolElems[L]
case class DeferredMap[L <: LA](name: String, customTpe: Option[L#Type])   extends LazyProtocolElems[L]

sealed trait StrictProtocolElems[L <: LA]                 extends ProtocolElems[L]
case class RandomType[L <: LA](name: String, tpe: L#Type) extends StrictProtocolElems[L]

sealed trait NestedProtocolElems[L <: LA] extends StrictProtocolElems[L]

case class ClassDefinition[L <: LA](
    name: String,
    tpe: L#TypeName,
    fullType: L#Type,
    cls: L#ClassDefinition,
    staticDefns: StaticDefns[L],
    parents: List[SuperClass[L]] = Nil
) extends NestedProtocolElems[L]

case class ADT[L <: LA](name: String, tpe: L#TypeName, fullType: L#Type, trt: L#Trait, staticDefns: StaticDefns[L]) extends StrictProtocolElems[L]

case class EnumDefinition[L <: LA](
    name: String,
    tpe: L#TypeName,
    fullType: L#Type,
    elems: RenderedEnum[L],
    cls: L#ClassDefinition,
    staticDefns: StaticDefns[L]
) extends NestedProtocolElems[L]

object ProtocolElems {
  def resolve[L <: LA, F[_]](
      elems: List[ProtocolElems[L]],
      limit: Int = 10
  )(implicit Sc: LanguageTerms[L, F], Cl: CollectionsLibTerms[L, F], Sw: SwaggerTerms[L, F], P: ProtocolSupportTerms[L, F]): F[List[StrictProtocolElems[L]]] = {
    import Sc._
    import Cl._
    import Sw._
    log.function(s"resolve(${elems.length} references)")(
      FlatMap[F]
        .tailRecM[(Int, List[ProtocolElems[L]]), List[StrictProtocolElems[L]]]((limit, elems))({
          case (iters, xs) if iters > 0 =>
            val lazyElems: List[LazyProtocolElems[L]]     = xs.collect { case x: LazyProtocolElems[_]   => x }
            val strictElems: List[StrictProtocolElems[L]] = xs.collect { case x: StrictProtocolElems[_] => x }
            for {
              _ <- log.debug(s"$iters left")
              res <- if (lazyElems.nonEmpty) {
                val newElems = lazyElems
                  .traverse[F, ProtocolElems[L]]({
                    case d @ Deferred(name) =>
                      strictElems
                        .find(_.name == name)
                        .fold[F[ProtocolElems[L]]](d.pure[F].widen)({
                          case RandomType(name, tpe) =>
                            RandomType[L](name, tpe).pure[F].widen
                          case ClassDefinition(name, tpe, _, cls, _, _) =>
                            widenTypeName(tpe).map(RandomType[L](name, _))
                          case EnumDefinition(name, tpe, _, _, cls, _) =>
                            widenTypeName(tpe).map(RandomType[L](name, _))
                          case ADT(name, tpe, _, _, _) =>
                            widenTypeName(tpe).map(RandomType[L](name, _))
                        })
                    case d @ DeferredArray(name, customTpe) =>
                      strictElems
                        .find(_.name == name)
                        .fold[F[ProtocolElems[L]]](d.pure[F].widen)({
                          case RandomType(name, tpe) =>
                            liftVectorType(tpe, customTpe).map(RandomType[L](name, _))
                          case ClassDefinition(name, tpe, _, cls, _, _) =>
                            widenTypeName(tpe).flatMap(liftVectorType(_, customTpe)).map(RandomType[L](name, _))
                          case EnumDefinition(name, tpe, _, _, cls, _) =>
                            widenTypeName(tpe).flatMap(liftVectorType(_, customTpe)).map(RandomType[L](name, _))
                          case ADT(name, tpe, _, _, _) =>
                            widenTypeName(tpe).flatMap(liftVectorType(_, customTpe)).map(RandomType[L](name, _))
                        })
                    case d @ DeferredMap(name, customTpe) =>
                      strictElems
                        .find(_.name == name)
                        .fold[F[ProtocolElems[L]]](d.pure[F].widen)({
                          case RandomType(name, tpe) =>
                            liftMapType(tpe, customTpe).map(RandomType[L](name, _))
                          case ClassDefinition(name, tpe, _, cls, _, _) =>
                            widenTypeName(tpe).flatMap(liftMapType(_, customTpe)).map(RandomType[L](name, _))
                          case EnumDefinition(name, tpe, _, _, cls, _) =>
                            widenTypeName(tpe).flatMap(liftMapType(_, customTpe)).map(RandomType[L](name, _))
                          case ADT(name, tpe, _, _, _) =>
                            widenTypeName(tpe).flatMap(liftMapType(_, customTpe)).map(RandomType[L](name, _))
                        })
                  })
                  .map(strictElems ++ _)
                newElems.map { x =>
                  Left((iters - 1, x))
                }
              } else Right(strictElems).pure[F].widen
            } yield res
          case (_, xs) =>
            val lazyElems = xs.collect { case x: LazyProtocolElems[_] => x }
            fallbackResolveElems(lazyElems).map(Right(_))
        })
    )
  }
}
