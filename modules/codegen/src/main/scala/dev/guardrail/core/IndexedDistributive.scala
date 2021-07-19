package dev.guardrail.core

import cats.{ Eval, FlatMap, Foldable, Functor, MonoidK }
import cats.syntax.all._

/** IndexedDistributive, heavily inspired by Data.Distributive, but exposing an index into the structure being walked
  *
  *  val result: List[(String, Tracker[Foo])] =
  *    IndexedDistributive[Tracker[List[(String, HasFoo)]]]
  *      .cotraverse(value)(_.downField("foo"), _.getFoo)
  */
trait IndexedDistributive[F[_]] {
  def indexedDistribute[G[_], A](value: F[G[A]])(implicit G: IndexedFunctor[G]): G[F[A]]
}

object IndexedDistributive {
  def apply[F[_]](implicit ev: IndexedDistributive[F]): IndexedDistributive[F] = ev

  def cotraverse[F[_], G[_]: IndexedFunctor, A, B](value: F[G[A]])(f: F[A] => B)(implicit F: IndexedDistributive[F], G: Functor[G]): G[B] =
    G.map(F.indexedDistribute(value))(f)
}

trait IndexedDistributiveImplicits {
  implicit class IndexedDistributiveSyntax[G[_]: Functor: IndexedFunctor: Foldable, F[_]: IndexedDistributive, A](value: F[G[A]]) {
    def indexedDistribute: G[F[A]]                                        = IndexedDistributive[F].indexedDistribute(value)
    def cotraverse[B](f: F[A] => B): G[B]                                 = IndexedDistributive.cotraverse(value)(f)
    def indexedCosequence: G[F[A]]                                        = cotraverse(identity)
    def flatCotraverse[B](f: F[A] => G[B])(implicit ev: FlatMap[G]): G[B] = ev.flatten(IndexedDistributive.cotraverse(value)(f))
    def flatExtract[B](f: F[A] => G[B])(implicit G: MonoidK[G]): G[B]     = value.cotraverse(f).foldLeft(G.empty[B])(G.combineK)
    def exists(f: F[A] => Boolean): Boolean                               = value.indexedDistribute.foldLeft(false) { case (acc, n) => acc || f(n) }
  }

  // These don't belong here, but they intersect a lot with the usage of IndexedDistributive
  implicit def nestedTupleFunctor[F[_], Z](implicit F: Functor[F]) = new Functor[Lambda[Y => F[(Z, Y)]]] {
    def map[A, B](fa: F[(Z, A)])(f: A => B): F[(Z, B)] = F.map(fa)(_.map(f))
  }

  implicit def nestedTupleFoldable[F[_], Z](implicit F: Foldable[F]) = new Foldable[Lambda[Y => F[(Z, Y)]]] {
    def foldLeft[A, B](fa: F[(Z, A)], b: B)(f: (B, A) => B): B                           = F.foldLeft(fa, b)({ case (b, (z, a))   => f(b, a) })
    def foldRight[A, B](fa: F[(Z, A)], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa, lb)({ case ((z, a), b) => f(a, b) })
  }
}
