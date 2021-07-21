package dev.guardrail.core

import cats.{ Eval, Foldable, Functor }

// Effectively Nested[F, (K, *), V]
// With some thought, this may just be expressible via a type alias
class Mappish[F[_], K, V](val value: F[(K, V)])

object Mappish {
  def apply[F[_], A, B](value: F[(A, B)]): Mappish[F, A, B] = new Mappish(value)
}

trait MappishImplicits {

  def buildMappishIndexedFunctor[F[_], Z](buildLabel: Z => String)(implicit F: IndexedFunctor[F]): IndexedFunctor[Mappish[F, Z, *]] =
    new IndexedFunctor[Mappish[F, Z, *]] {
      type I = Z
      def map[A, B](fa: Mappish[F, I, A])(f: (I, A) => B): Mappish[F, I, B] = Mappish(F.map(fa.value)({ case (_, (z, a)) => (z, f(z, a)) }))
      def label(i: I): Option[String]                                       = Some(buildLabel(i))
    }

  implicit def stringMIF[F[_]: IndexedFunctor]     = buildMappishIndexedFunctor[F, String](x => s".$x")
  implicit def httpMethodMIF[F[_]: IndexedFunctor] = buildMappishIndexedFunctor[F, io.swagger.v3.oas.models.PathItem.HttpMethod](x => s".${x.name}")

  implicit def mappishFunctor[F[_], Z](implicit F: Functor[F]): Functor[Mappish[F, Z, *]] = new Functor[Mappish[F, Z, *]] {
    def map[A, B](fa: Mappish[F, Z, A])(f: A => B): Mappish[F, Z, B] = Mappish(F.map(fa.value)({ case (z, a) => (z, f(a)) }))
  }

  implicit def mappishFoldable[F[_], Z](implicit F: Foldable[F]): Foldable[Mappish[F, Z, *]] = new Foldable[Mappish[F, Z, *]] {
    def foldLeft[A, B](fa: Mappish[F, Z, A], b: B)(f: (B, A) => B): B                           = F.foldLeft(fa.value, b)({ case (b, (z, a))   => f(b, a) })
    def foldRight[A, B](fa: Mappish[F, Z, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = F.foldRight(fa.value, lb)({ case ((z, a), b) => f(a, b) })
  }
}
