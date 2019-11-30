package com.twilio.guardrail

import cats.FlatMap
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import com.twilio.guardrail.protocol.terms.protocol.ProtocolSupportTerms

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
    elems: List[(String, L#TermName, L#TermSelect)],
    cls: L#ClassDefinition,
    staticDefns: StaticDefns[L]
) extends NestedProtocolElems[L]

object ProtocolElems {
  def resolve[L <: LA, F[_]](
      elems: List[ProtocolElems[L]],
      limit: Int = 10
  )(implicit Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F], P: ProtocolSupportTerms[L, F]): Free[F, List[StrictProtocolElems[L]]] = {
    import Sc._
    import Sw._
    log.function(s"resolve(${elems.length} references)")(
      FlatMap[Free[F, ?]]
        .tailRecM[(Int, List[ProtocolElems[L]]), List[StrictProtocolElems[L]]]((limit, elems))({
          case (iters, xs) if iters > 0 =>
            val lazyElems: List[LazyProtocolElems[L]]     = xs.collect { case x: LazyProtocolElems[_]   => x }
            val strictElems: List[StrictProtocolElems[L]] = xs.collect { case x: StrictProtocolElems[_] => x }
            for {
              _ <- log.debug(s"$iters left")
              res <- if (lazyElems.nonEmpty) {
                val newElems = lazyElems
                  .traverse[Free[F, ?], ProtocolElems[L]]({
                    case d @ Deferred(name) =>
                      strictElems
                        .find(_.name == name)
                        .fold[Free[F, ProtocolElems[L]]](Free.pure(d))({
                          case RandomType(name, tpe) =>
                            Free.pure(RandomType[L](name, tpe))
                          case ClassDefinition(name, tpe, _, cls, _, _) =>
                            widenTypeName(tpe).map(RandomType[L](name, _))
                          case EnumDefinition(name, tpe, _, elems, cls, _) =>
                            widenTypeName(tpe).map(RandomType[L](name, _))
                          case ADT(name, tpe, _, _, _) =>
                            widenTypeName(tpe).map(RandomType[L](name, _))
                        })
                    case d @ DeferredArray(name, customTpe) =>
                      strictElems
                        .find(_.name == name)
                        .fold[Free[F, ProtocolElems[L]]](Free.pure(d))({
                          case RandomType(name, tpe) =>
                            liftVectorType(tpe, customTpe).map(RandomType[L](name, _))
                          case ClassDefinition(name, tpe, _, cls, _, _) =>
                            widenTypeName(tpe).flatMap(liftVectorType(_, customTpe)).map(RandomType[L](name, _))
                          case EnumDefinition(name, tpe, _, elems, cls, _) =>
                            widenTypeName(tpe).flatMap(liftVectorType(_, customTpe)).map(RandomType[L](name, _))
                          case ADT(name, tpe, _, _, _) =>
                            widenTypeName(tpe).flatMap(liftVectorType(_, customTpe)).map(RandomType[L](name, _))
                        })
                    case d @ DeferredMap(name, customTpe) =>
                      strictElems
                        .find(_.name == name)
                        .fold[Free[F, ProtocolElems[L]]](Free.pure(d))({
                          case RandomType(name, tpe) =>
                            liftMapType(tpe, customTpe).map(RandomType[L](name, _))
                          case ClassDefinition(name, tpe, _, cls, _, _) =>
                            widenTypeName(tpe).flatMap(liftMapType(_, customTpe)).map(RandomType[L](name, _))
                          case EnumDefinition(name, tpe, _, elems, cls, _) =>
                            widenTypeName(tpe).flatMap(liftMapType(_, customTpe)).map(RandomType[L](name, _))
                          case ADT(name, tpe, _, _, _) =>
                            widenTypeName(tpe).flatMap(liftMapType(_, customTpe)).map(RandomType[L](name, _))
                        })
                  })
                  .map(strictElems ++ _)
                newElems.map { x =>
                  Left((iters - 1, x))
                }
              } else Free.pure[F, Either[(Int, List[ProtocolElems[L]]), List[StrictProtocolElems[L]]]](Right(strictElems))
            } yield res
          case (_, xs) =>
            val lazyElems = xs.collect { case x: LazyProtocolElems[_] => x }
            fallbackResolveElems(lazyElems).map(Right(_))
        })
    )
  }
}
