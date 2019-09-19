package com.twilio.guardrail.core

import _root_.io.swagger.v3.oas.models.OpenAPI
import cats._
import cats.data._
import cats.implicits._
import com.twilio.guardrail.Target
import scala.collection.JavaConverters._

class Tracker[+A] private[core] (private[core] val get: A, private[core] val history: Vector[String]) {
  override def toString(): String = s"Tracker($get, $history)"
  override def hashCode(): Int    = get.hashCode // FIXME: Are two things with different histories identical?
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
  implicit def optionaljuMapConvincer[K, V]: Tracker.Convincer[Option[java.util.Map[K, V]], List[(K, V)]] =
    Tracker.Convincer(_.fold(List.empty[(K, V)])(_.asScala.toList))
  implicit def optionalMapConvincer[K, V]: Tracker.Convincer[Option[Map[K, V]], List[(K, V)]] = Tracker.Convincer(_.fold(List.empty[(K, V)])(_.toList))
}

trait LowPrioritySyntax {
  implicit class ListSyntax[A](tracker: Tracker[List[A]]) {
    def exists(f: Tracker[A] => Boolean): Boolean = sequence.exists(f)
    def sequence: List[Tracker[A]]                = extract(identity _)
    def extract[B](f: Tracker[A] => B): List[B]   = flatExtract(a => List(f(a)))
    def flatExtract[F[_]: MonoidK, B](f: Tracker[A] => F[B]): F[B] =
      tracker.get.zipWithIndex
        .foldLeft(MonoidK[F].empty[B]) {
          case (acc, (x, idx)) =>
            MonoidK[F].combineK(acc, f(new Tracker(x, tracker.history :+ s"[${idx}]")))
        }

    def toNel: Tracker[Option[NonEmptyList[A]]] = tracker.map(NonEmptyList.fromList _)
  }

  implicit class NELSyntax[A](tracker: Tracker[NonEmptyList[A]]) {
    def foldExtract[F[_], B](f: Tracker[A] => F[B])(combine: (F[B], F[B]) => F[B]): F[B] =
      tracker
        .get
        .zipWithIndex
        .map({ case (v, i) => f(new Tracker(v, tracker.history :+ s"[${i}]")) })
        .reduceLeft(combine)
    def sequence: NonEmptyList[Tracker[A]] = foldExtract(v => NonEmptyList.one(v))((a, b) => a.concatNel(b))
  }
}

object Tracker extends HighPriorityTrackerEvidence with LowPrioritySyntax {
  object Convincer {
    def apply[A, B](f: A => B) = new Convincer[A, B] {
      def apply(a: A): B = f(a)
    }
  }
  sealed trait Convincer[-A, +B] {
    def apply(a: A): B
  }

  def apply(swagger: OpenAPI): Tracker[OpenAPI]                    = new Tracker(swagger, Vector.empty)
  def hackyAdapt[A](value: A, history: Vector[String]): Tracker[A] = new Tracker(value, history)

  implicit class OptionSyntax[A](tracker: Tracker[Option[A]]) {
    def sequence: Option[Tracker[A]]                       = tracker.get.map(new Tracker(_, tracker.history))
    def orHistory: Either[Vector[String], Tracker[A]]      = tracker.sequence.toRight(tracker.history)
    def raiseErrorIfEmpty(err: String): Target[Tracker[A]] = Target.fromOption(tracker.sequence, s"${err} (${tracker.showHistory})")
    class FlatDownFieldPartiallyApplied[C](val dummy: Boolean = true) {
      def apply[B](label: String, f: A => B)(implicit ev: Convincer[Option[B], C]): Tracker[C] =
        new Tracker(ev(tracker.get.flatMap(x => Option(f(x)))), tracker.history :+ s".${label}")
    }
    def flatDownField[C]: FlatDownFieldPartiallyApplied[C] = new FlatDownFieldPartiallyApplied()
    def flatExtract[F[_]: MonoidK, B](f: Tracker[A] => F[B]): F[B] =
      tracker.get.fold(MonoidK[F].empty[B])(x => f(tracker.map(_ => x)))
    def exists(f: Tracker[A] => Boolean): Boolean = sequence.exists(f)
  }

  implicit class MapishListSyntax[K, V](tracker: Tracker[List[(K, V)]]) {
    def foldExtract[F[_], B](zero: F[B])(combine: (F[B], F[B]) => F[B])(f: (K, Tracker[V]) => F[B]): F[B] =
      tracker.get
        .foldLeft(zero) {
          case (acc, (k, v)) =>
            combine(acc, f(k, new Tracker(v, tracker.history :+ s"[${k}]")))
        }
    def sequence: List[(K, Tracker[V])] = foldExtract(List.empty[(K, Tracker[V])])(_ ++ _)((k, v) => List(k -> v))
  }

  implicit class MapishNELSyntax[K, V](tracker: Tracker[NonEmptyList[(K, V)]]) {
    def foldExtract[F[_], B](f: (K, Tracker[V]) => F[B])(combine: (F[B], F[B]) => F[B]): F[B] =
      tracker.get
        .map({ case (k, v) => f(k, new Tracker(v, tracker.history :+ s"[${k}]")) })
        .reduceLeft(combine)
    def sequence: NonEmptyList[(K, Tracker[V])] = foldExtract((k, v) => NonEmptyList.one((k, v)))((a, b) => a.concatNel(b))
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
      def apply[B](label: String, f: A => B)(implicit ev: Convincer[Option[B], C]): Tracker[C] =
        new Tracker(ev(Option(f(tracker.get))), tracker.history :+ s".${label}")
    }
    def downField[C]: DownFieldPartiallyApplied[C] = new DownFieldPartiallyApplied()

    def map[B](f: A => B): Tracker[B] = new Tracker(f(tracker.get), tracker.history)

    def showHistory: String = tracker.history.mkString("")

    @deprecated("Tracker.get will be removed once the migration has been completed. Please use the fold/traverse combinators to get values out.", "0.0.0")
    def get: A = tracker.get

    @deprecated("Tracker.get will be removed once the migration has been completed. Please use the fold/traverse combinators to get values out.", "0.0.0")
    def history: Vector[String] = tracker.history
  }
}
