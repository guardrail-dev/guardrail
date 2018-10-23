package com.twilio.guardrail.swagger

import com.twilio.guardrail._
import cats.instances.list._
import cats.syntax.either._
import cats.{ FlatMap, Foldable }
import scala.meta.{ Term, Type }
import scala.meta._

sealed trait ResolvedType

object ResolvedType {
  case class Resolved(tpe: Type, classDep: Option[Term.Name], defaultValue: Option[Term]) extends ResolvedType

  sealed trait LazyResolvedType extends ResolvedType

  case class Deferred(value: String)                   extends LazyResolvedType
  case class DeferredComp(value: String, pRef: String) extends LazyResolvedType
  case class DeferredArray(value: String)              extends LazyResolvedType
  case class DeferredMap(value: String)                extends LazyResolvedType

  implicit class FoldableExtension[F[_]](F: Foldable[F]) {
    import cats.{ Alternative, Monoid }
    def partitionEither[A, B, C](value: F[A])(f: A => Either[B, C])(implicit A: Alternative[F]): (F[B], F[C]) = {
      import cats.instances.tuple._

      implicit val mb: Monoid[F[B]] = A.algebra[B]
      implicit val mc: Monoid[F[C]] = A.algebra[C]

      F.foldMap(value) { a =>
        f(a) match {
          case Left(b)  => (A.pure(b), A.empty[C])
          case Right(c) => (A.empty[B], A.pure(c))
        }
      }
    }
  }

//  def resolve_(values: List[(String, ResolvedType)]): Target[List[(String, Resolved)]] = {
//    val (lazyTypes, resolvedTypes) = Foldable[List].partitionEither(values) {
//      case (clsName, x: Resolved)         => Right((clsName, x))
//      case (clsName, x: LazyResolvedType) => Left((clsName, x))
//    }
//
//    def lookupTypeName(clsName: String, tpeName: String, resolvedTypes: List[(String, Resolved)])(f: Type => Type): Option[(String, Resolved)] =
//      resolvedTypes
//        .find(_._1 == tpeName)
//        .map(_._2.tpe)
//        .map(x => (clsName, Resolved(f(x), None, None)))
//
//    FlatMap[Target]
//      .tailRecM[(List[(String, LazyResolvedType)], List[(String, Resolved)]), List[(String, Resolved)]]((lazyTypes, resolvedTypes)) {
//        case (lazyTypes, resolvedTypes) =>
//          if (lazyTypes.isEmpty) {
//            Target.pure(Right(resolvedTypes))
//          } else {
//            val (newLazyTypes, newResolvedTypes) =
//              Foldable[List].partitionEither(lazyTypes) {
//                case x @ (clsName, Deferred(tpeName)) =>
//                  Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(identity), x)
//                case x @ (clsName, DeferredArray(tpeName)) =>
//                  Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(tpe => t"IndexedSeq[${tpe}]"), x)
//                case x @ (clsName, DeferredMap(tpeName)) =>
//                  Either.fromOption(lookupTypeName(clsName, tpeName, resolvedTypes)(tpe => t"Map[String, ${tpe}]"), x)
//              }
//
//            Target.pure(Left((newLazyTypes, resolvedTypes ++ newResolvedTypes)))
//          }
//      }
//  }

  def resolve(value: ResolvedType, protocolElems: List[StrictProtocolElems]): Target[Resolved] =
    value match {
      case comp: DeferredComp =>
        // todo

        Target
          .fromOption(protocolElems.find(_.name == comp.value), s"Unable to resolve ${comp.value}")
          .map {
            case RandomType(_, tpe) => Resolved(tpe, None, None)
            case ClassDefinition(_, tpe, _, _, _) =>
              Resolved(tpe, None, None)
            case EnumDefinition(_, tpe, _, _, _) =>
              Resolved(tpe, None, None)
            case ADT(_, tpe, _, _) =>
              Resolved(tpe, None, None)
          }

      case x @ Resolved(_, _, _) => Target.pure(x)
      case Deferred(name) =>
        Target
          .fromOption(protocolElems.find(_.name == name), s"Unable to resolve $name")
          .map {
            case RandomType(_, tpe) => Resolved(tpe, None, None)
            case ClassDefinition(_, tpe, _, _, _) =>
              Resolved(tpe, None, None)
            case EnumDefinition(_, tpe, _, _, _) =>
              Resolved(tpe, None, None)
            case ADT(_, tpe, _, _) =>
              Resolved(tpe, None, None)
          }
      case DeferredArray(name) =>
        Target
          .fromOption(protocolElems.find(_.name == name), s"Unable to resolve $name")
          .map {
            case RandomType(name, tpe) =>
              Resolved(t"IndexedSeq[$tpe]", None, None)
            case ClassDefinition(name, tpe, cls, companion, _) =>
              Resolved(t"IndexedSeq[$tpe]", None, None)
            case EnumDefinition(name, tpe, elems, cls, companion) =>
              Resolved(t"IndexedSeq[$tpe]", None, None)
            case ADT(_, tpe, _, _) =>
              Resolved(t"IndexedSeq[$tpe]", None, None)
          }
      case DeferredMap(name) =>
        Target
          .fromOption(protocolElems.find(_.name == name), s"Unable to resolve $name")
          .map {
            case RandomType(name, tpe) =>
              Resolved(t"Map[String, $tpe]", None, None)
            case ClassDefinition(_, tpe, _, _, _) =>
              Resolved(t"Map[String, $tpe]", None, None)
            case EnumDefinition(_, tpe, _, _, _) =>
              Resolved(t"Map[String, $tpe]", None, None)
            case ADT(_, tpe, _, _) =>
              Resolved(t"Map[String, $tpe]", None, None)
          }
    }
}
