package dev.guardrail.core

import cats.data._

/** IndexedFunctor, similar to Functor except exposing an index during map.
  *
  *  Additionally, label(i: I): Option[String] may convert the exposed index into a string
  */
trait IndexedFunctor[F[_]] {
  type I
  def map[A, B](fa: F[A])(f: (I, A) => B): F[B]
  def label(i: I): Option[String]
}

object IndexedFunctor {
  implicit def indexedMap: IndexedFunctor[Map[String, *]] = new IndexedFunctor[Map[String, *]] {
    type I = String
    def map[A, B](fa: Map[String, A])(f: (I, A) => B): Map[String, B] = fa.map({ case (k, v) => (k, f(k, v)) })
    def label(i: String)                                              = Some('.' +: i)
  }

  implicit object indexedList extends IndexedFunctor[List] {
    type I = Int
    def map[A, B](fa: List[A])(f: (I, A) => B): List[B] = fa.zipWithIndex.map({ case (v, k) => f(k, v) })
    def label(i: Int): Some[String]                     = Some(s"[${i.toString()}]")
  }

  implicit object indexedNonEmptyList extends IndexedFunctor[NonEmptyList] {
    type I = Int
    def map[A, B](fa: NonEmptyList[A])(f: (I, A) => B): NonEmptyList[B] = fa.zipWithIndex.map({ case (v, k) => f(k, v) })
    def label(i: Int): Some[String]                                     = Some(s"[${i.toString()}]")
  }

  implicit def indexedOption: IndexedFunctor[Option] = new IndexedFunctor[Option] {
    type I = Unit
    def map[A, B](fa: Option[A])(f: (I, A) => B): Option[B] = fa.map(f((), _))
    def label(i: Unit)                                      = None
  }
}
