package com.twilio.guardrail.core

import _root_.io.swagger.v3.oas.models.OpenAPI
import cats.data._
import cats.implicits._
import com.twilio.guardrail.Target
import scala.collection.JavaConverters._

class Tracker[+A] private[core] (private[core] val get: A, private[core] val history: Vector[String]) {
  override def toString(): String = s"Tracker($get, $history)"
}

trait LowPriorityTrackerEvidence {
  implicit def arbConvincer[A]: Tracker.Convincer[A, A] = Tracker.Convincer(identity _)
}

trait HighPriorityTrackerEvidence extends LowPriorityTrackerEvidence {
  implicit def optionalArrayConvincer[A]: Tracker.Convincer[Option[Array[A]], List[A]]           = Tracker.Convincer(_.fold(List.empty[A])(_.toList))
  implicit def optionaljuListConvincer[A]: Tracker.Convincer[Option[java.util.List[A]], List[A]] = Tracker.Convincer(_.fold(List.empty[A])(_.asScala.toList))
  implicit def optionalListConvincer[A]: Tracker.Convincer[Option[List[A]], List[A]]             = Tracker.Convincer(_.getOrElse(List.empty[A]))
  implicit def optionaljuCollectionConvincer[A]: Tracker.Convincer[Option[java.util.Collection[A]], List[A]] =
    Tracker.Convincer(_.fold(List.empty[A])(_.asScala.toList))
  implicit def optionalOptionConvincer[A]: Tracker.Convincer[Option[Option[A]], Option[A]] = Tracker.Convincer(_.flatten)
  implicit def optionaljuMapConvincer[K, V]: Tracker.Convincer[Option[java.util.Map[K, V]], Mappish[List, K, V]] =
    Tracker.Convincer(_.fold(Mappish(List.empty[(K, V)]))(x => Mappish(x.asScala.toList)))
  implicit def optionalMapConvincer[K, V]: Tracker.Convincer[Option[Map[K, V]], Mappish[List, K, V]] =
    Tracker.Convincer(_.fold(Mappish(List.empty[(K, V)]))(x => Mappish(x.toList)))
}

trait LowestPriorityTrackerInstances {
  implicit object distributiveTracker extends IndexedDistributive[Tracker] {
    def indexedDistribute[G[_], A](value: Tracker[G[A]])(implicit G: IndexedFunctor[G]): G[Tracker[A]] =
      G.map(value.unwrapTracker) {
        case (i, a) =>
          new Tracker(a, value.history ++ G.label(i))
      }
  }

  implicit def trackerFunctor = new cats.Functor[Tracker] {
    def map[A, B](fa: Tracker[A])(f: A => B): Tracker[B] = new Tracker(f(fa.unwrapTracker), fa.history)
  }
}

trait LowPriorityTrackerSyntax extends LowestPriorityTrackerInstances {
  implicit class ListSyntax[A](tracker: Tracker[List[A]]) {
    def toNel: Tracker[Option[NonEmptyList[A]]] = tracker.map(NonEmptyList.fromList _)
  }

  implicit class MappishSyntax[A, B](tracker: Tracker[Mappish[List, A, B]]) {
    def toNel: Tracker[Option[Mappish[NonEmptyList, A, B]]] = tracker.map(x => NonEmptyList.fromList(x.value).map(Mappish(_)))
  }
}

trait HighPriorityTrackerSyntax extends LowPriorityTrackerSyntax {
  implicit class StringyEitherSyntax[B](tracker: Tracker[Either[String, B]]) {
    def raiseErrorIfLeft: Target[Tracker[B]] = tracker.fold(err => Target.raiseError(s"${err.get} (${err.showHistory})"), Target.pure _)
  }

  implicit class EitherSyntax[A, B](tracker: Tracker[Either[A, B]]) {
    def fold[C](a: Tracker[A] => C, b: Tracker[B] => C): C =
      tracker.unwrapTracker.fold(x => a(Tracker.cloneHistory(tracker, x)), x => b(Tracker.cloneHistory(tracker, x)))
  }

  implicit class OptionSyntax[A](tracker: Tracker[Option[A]]) {
    def orHistory: Either[Vector[String], Tracker[A]]      = tracker.indexedCosequence.toRight(tracker.history)
    def raiseErrorIfEmpty(err: String): Target[Tracker[A]] = Target.fromOption(tracker.indexedCosequence, s"${err} (${tracker.showHistory})")
    class FlatDownFieldPartiallyApplied[C](val dummy: Boolean = true) {
      def apply[B](label: String, f: A => B)(implicit ev: Tracker.Convincer[Option[B], C]): Tracker[C] =
        new Tracker(ev(tracker.get.flatMap(x => Option(f(x)))), tracker.history :+ s".${label}")
    }
    def flatDownField[C]: FlatDownFieldPartiallyApplied[C] = new FlatDownFieldPartiallyApplied()
  }

  implicit class RefineSyntax[A](tracker: Tracker[A]) {
    class RefinePartiallyApplied[C](val dummy: Boolean = true) {
      def apply[B1](r: PartialFunction[A, B1])(f: Tracker[B1] => C): Either[Tracker[A], C] =
        r.andThen(value => f(tracker.map(_ => value)))
          .andThen(Right(_))
          .applyOrElse(tracker.get, (_: A) => Left(tracker))
    }
    def refine[C]: RefinePartiallyApplied[C] = new RefinePartiallyApplied[C]()
  }

  implicit class RefineEitherSyntax[A, C](value: Either[Tracker[A], C]) {
    def orRefine[B1](r: PartialFunction[A, B1])(f: Tracker[B1] => C): Either[Tracker[A], C] =
      value.fold({ tracker =>
        r.andThen(value => f(tracker.map(_ => value)))
          .andThen(Right(_))
          .applyOrElse(tracker.get, (_: A) => Left(tracker))
      }, Right(_))
  }

  implicit class Syntax[A](tracker: Tracker[A]) {
    class DownFieldPartiallyApplied[C](val dummy: Boolean = true) {
      def apply[B](label: String, f: A => B)(implicit ev: Tracker.Convincer[Option[B], C]): Tracker[C] =
        new Tracker(ev(Option(f(tracker.get))), tracker.history :+ s".${label}")
    }
    def downField[C]: DownFieldPartiallyApplied[C] = new DownFieldPartiallyApplied()

    def showHistory: String = tracker.history.mkString("")

    @deprecated("Tracker.get will be removed once the migration has been completed. Please use the fold/traverse combinators to get values out.", "0.0.0")
    def get: A = tracker.get

    def unwrapTracker: A = tracker.get

    @deprecated("Tracker.get will be removed once the migration has been completed. Please use the fold/traverse combinators to get values out.", "0.0.0")
    def history: Vector[String] = tracker.history
  }
}

object Tracker extends HighPriorityTrackerEvidence with HighPriorityTrackerSyntax {
  object Convincer {
    def apply[A, B](f: A => B) = new Convincer[A, B] {
      def apply(a: A): B = f(a)
    }
  }
  sealed trait Convincer[-A, +B] {
    def apply(a: A): B
  }

  def apply(swagger: OpenAPI): Tracker[OpenAPI]                     = new Tracker(swagger, Vector.empty)
  def hackyAdapt[A](value: A, history: Vector[String]): Tracker[A]  = new Tracker(value, history)
  def cloneHistory[A, B](tracker: Tracker[A], value: B): Tracker[B] = new Tracker(value, tracker.history)
}
