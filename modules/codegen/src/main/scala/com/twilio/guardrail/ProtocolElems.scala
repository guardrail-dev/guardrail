package com.twilio.guardrail

import cats.MonadError
import scala.meta._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.languages.ScalaLanguage

sealed trait ProtocolElems[L <: LA]

sealed trait LazyProtocolElems[L <: LA]         extends ProtocolElems[L] { def name: String }
case class Deferred[L <: LA](name: String)      extends LazyProtocolElems[L]
case class DeferredArray[L <: LA](name: String) extends LazyProtocolElems[L]
case class DeferredMap[L <: LA](name: String)   extends LazyProtocolElems[L]

sealed trait StrictProtocolElems[L <: LA]                 extends ProtocolElems[L] { def name: String }
case class RandomType[L <: LA](name: String, tpe: L#Type) extends StrictProtocolElems[L]
case class ClassDefinition[L <: LA](name: String, tpe: L#TypeName, cls: L#ClassDefinition, companion: L#ObjectDefinition, parents: List[SuperClass[L]] = Nil)
    extends StrictProtocolElems[L]

case class ADT[L <: LA](name: String, tpe: L#TypeName, trt: L#Trait, companion: L#ObjectDefinition) extends StrictProtocolElems[L]

case class EnumDefinition[L <: LA](
    name: String,
    tpe: L#TypeName,
    elems: List[(String, L#TermName, Term.Select)],
    cls: Defn.Class,
    companion: Defn.Object
) extends StrictProtocolElems[L]

object ProtocolElems {
  def resolve[F[_]](elems: List[ProtocolElems[ScalaLanguage]],
                    limit: Int = 10)(implicit M: MonadError[F, String]): F[List[StrictProtocolElems[ScalaLanguage]]] =
    M.tailRecM[(Int, List[ProtocolElems[ScalaLanguage]]), List[StrictProtocolElems[ScalaLanguage]]]((limit, elems))({
      case (iters, xs) if iters > 0 =>
        val lazyElems: List[LazyProtocolElems[ScalaLanguage]]     = xs.collect { case x: LazyProtocolElems[_]   => x }
        val strictElems: List[StrictProtocolElems[ScalaLanguage]] = xs.collect { case x: StrictProtocolElems[_] => x }
        if (lazyElems.nonEmpty) {
          val newElems: List[ProtocolElems[ScalaLanguage]] = strictElems ++ lazyElems.map {
            case d @ Deferred(name) =>
              strictElems
                .find(_.name == name)
                .map({
                  case RandomType(name, tpe) => RandomType[ScalaLanguage](name, tpe)
                  case ClassDefinition(name, tpe, cls, companion, _) =>
                    RandomType[ScalaLanguage](name, tpe)
                  case EnumDefinition(name, tpe, elems, cls, companion) =>
                    RandomType[ScalaLanguage](name, tpe)
                  case ADT(name, tpe, _, _) =>
                    RandomType[ScalaLanguage](name, tpe)
                })
                .getOrElse(d)
            case d @ DeferredArray(name) =>
              strictElems
                .find(_.name == name)
                .map({
                  case RandomType(name, tpe) =>
                    RandomType[ScalaLanguage](name, t"IndexedSeq[${tpe}]")
                  case ClassDefinition(name, tpe, cls, companion, _) =>
                    RandomType[ScalaLanguage](name, t"IndexedSeq[${tpe}]")
                  case EnumDefinition(name, tpe, elems, cls, companion) =>
                    RandomType[ScalaLanguage](name, t"IndexedSeq[${tpe}]")
                  case ADT(name, tpe, _, _) =>
                    RandomType[ScalaLanguage](name, t"IndexedSeq[${tpe}]")
                })
                .getOrElse(d)
            case d @ DeferredMap(name) =>
              strictElems
                .find(_.name == name)
                .map({
                  case RandomType(name, tpe) =>
                    RandomType[ScalaLanguage](name, t"Map[String, ${tpe}]")
                  case ClassDefinition(name, tpe, cls, companion, _) =>
                    RandomType[ScalaLanguage](name, t"Map[String, ${tpe}]")
                  case EnumDefinition(name, tpe, elems, cls, companion) =>
                    RandomType[ScalaLanguage](name, t"Map[String, ${tpe}]")
                  case ADT(name, tpe, _, _) =>
                    RandomType[ScalaLanguage](name, t"Map[String, ${tpe}]")
                })
                .getOrElse(d)
          }
          val next = (iters - 1, newElems)
          M.pure(Left(next))
        } else M.pure(Right(strictElems))
      case (_, xs) =>
        val lazyElems = xs.collect { case x: LazyProtocolElems[_] => x }
        M.raiseError(s"Unable to resolve: ${lazyElems.map(_.name)}")
    })
}
