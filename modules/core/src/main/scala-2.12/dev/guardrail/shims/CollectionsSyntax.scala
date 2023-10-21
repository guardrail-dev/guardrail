package dev.guardrail.shims

import scala.collection.IterableView

trait CollectionsSyntax {
  implicit class ListGroupMap[A](val xs: List[A]) {
    def groupMap[K, B](toKey: A => K)(toValue: A => B): Map[K, List[B]] =
      xs.groupBy(toKey).view.map { case (k, as) => (k, as.map(toValue)) }.toMap
  }

  implicit class IterableViewMapValues[K, A](val xs: IterableView[(K, A), Iterable[(K, A)]]) {
    def mapValues[B](f: A => B): IterableView[(K, B), Iterable[_]] =
      xs.map[(K, B), scala.collection.IterableView[(K, B), Iterable[_]]] { case (k, a) => (k, f(a)) }
  }
}
