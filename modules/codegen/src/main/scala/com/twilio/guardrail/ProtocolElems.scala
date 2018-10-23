package com.twilio.guardrail

import cats.MonadError
import scala.meta._

sealed trait ProtocolElems

sealed trait LazyProtocolElems         extends ProtocolElems { def name: String }
case class Deferred(name: String)      extends LazyProtocolElems
case class DeferredArray(name: String) extends LazyProtocolElems
case class DeferredMap(name: String)   extends LazyProtocolElems

sealed trait StrictProtocolElems                                                                                                   extends ProtocolElems { def name: String }
case class RandomType(name: String, tpe: Type)                                                                                     extends StrictProtocolElems
case class ClassDefinition(name: String, tpe: Type.Name, cls: Defn.Class, companion: Defn.Object, parents: List[SuperClass] = Nil) extends StrictProtocolElems

case class ADT(name: String, tpe: Type.Name, trt: Defn.Trait, companion: Defn.Object) extends StrictProtocolElems

case class EnumDefinition(
    name: String,
    tpe: Type.Name,
    elems: List[(String, Term.Name, Term.Select)],
    cls: Defn.Class,
    companion: Defn.Object
) extends StrictProtocolElems

object ProtocolElems {
  def resolve[F[_]](elems: List[ProtocolElems], limit: Int = 10)(implicit M: MonadError[F, String]): F[List[StrictProtocolElems]] =
    M.tailRecM[(Int, List[ProtocolElems]), List[StrictProtocolElems]]((limit, elems))({
      case (iters, xs) if iters > 0 =>
        val lazyElems   = xs.collect { case x: LazyProtocolElems   => x }
        val strictElems = xs.collect { case x: StrictProtocolElems => x }
        if (lazyElems.nonEmpty) {
          val newElems = strictElems ++ lazyElems.map {
            case d @ Deferred(name) =>
              strictElems
                .find(_.name == name)
                .map({
                  case RandomType(name, tpe) => RandomType(name, tpe)
                  case ClassDefinition(name, tpe, cls, companion, _) =>
                    RandomType(name, tpe)
                  case EnumDefinition(name, tpe, elems, cls, companion) =>
                    RandomType(name, tpe)
                  case ADT(name, tpe, _, _) =>
                    RandomType(name, tpe)
                })
                .getOrElse(d)
            case d @ DeferredArray(name) =>
              strictElems
                .find(_.name == name)
                .map({
                  case RandomType(name, tpe) =>
                    RandomType(name, t"IndexedSeq[${tpe}]")
                  case ClassDefinition(name, tpe, cls, companion, _) =>
                    RandomType(name, t"IndexedSeq[${tpe}]")
                  case EnumDefinition(name, tpe, elems, cls, companion) =>
                    RandomType(name, t"IndexedSeq[${tpe}]")
                  case ADT(name, tpe, _, _) =>
                    RandomType(name, t"IndexedSeq[$tpe]")
                })
                .getOrElse(d)
            case d @ DeferredMap(name) =>
              strictElems
                .find(_.name == name)
                .map({
                  case RandomType(name, tpe) =>
                    RandomType(name, t"Map[String, ${tpe}]")
                  case ClassDefinition(name, tpe, cls, companion, _) =>
                    RandomType(name, t"Map[String, ${tpe}]")
                  case EnumDefinition(name, tpe, elems, cls, companion) =>
                    RandomType(name, t"Map[String, ${tpe}]")
                  case ADT(name, tpe, _, _) =>
                    RandomType(name, t"Map[String, $tpe]")
                })
                .getOrElse(d)
          }
          val next = (iters - 1, newElems)
          M.pure(Left(next))
        } else M.pure(Right(strictElems))
      case (_, xs) =>
        val lazyElems = xs.collect { case x: LazyProtocolElems => x }
        M.raiseError(s"Unable to resolve: ${lazyElems.map(_.name)}")
    })
}
