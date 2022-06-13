package dev.guardrail.generators.spi

import cats.Monad
import cats.Monoid
import cats.Semigroup
import cats.StackSafeMonad
import cats.data.NonEmptyList
import cats.implicits._

object ModuleLoadResult {
  implicit val moduleLoadResultMonad: Monad[ModuleLoadResult] = new StackSafeMonad[ModuleLoadResult] {
    def pure[A](x: A): ModuleLoadResult[A] = new ModuleLoadSuccess[A](Set.empty, Set.empty, Set.empty, x)

    override def map[A, B](fa: ModuleLoadResult[A])(f: A => B) = fa match {
      case success: ModuleLoadSuccess[_] => new ModuleLoadSuccess[B](success.attempted, success.consumed, success.missing, f(success.result))
      case fail: ModuleLoadFailed        => fail
    }

    def flatMap[A, B](fa: ModuleLoadResult[A])(f: A => ModuleLoadResult[B]) =
      fa match {
        case failed: ModuleLoadFailed => failed
        case success: ModuleLoadSuccess[_] =>
          f(success.result) match {
            case next: ModuleLoadFailed =>
              new ModuleLoadFailed(success.attempted ++ next.attempted, success.consumed ++ next.consumed, success.missing ++ next.missing)
            case next: ModuleLoadSuccess[_] =>
              new ModuleLoadSuccess[B](success.attempted ++ next.attempted, success.consumed ++ next.consumed, success.missing ++ next.missing, next.result)
          }
      }
  }

  implicit def moduleLoadResultSemigroup[A]: Monoid[ModuleLoadResult[A]] = new Monoid[ModuleLoadResult[A]] {
    def empty: ModuleLoadResult[A] = new ModuleLoadFailed(Set.empty, Set.empty, Set.empty)
    def combine(a: ModuleLoadResult[A], b: ModuleLoadResult[A]): ModuleLoadResult[A] = (a, b) match {
      case (a: ModuleLoadSuccess[A], b: ModuleLoadSuccess[A]) =>
        new ModuleLoadSuccess(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing, a.result)
      case (a: ModuleLoadFailed, b: ModuleLoadSuccess[A]) =>
        new ModuleLoadSuccess(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing, b.result)
      case (a: ModuleLoadSuccess[A], b: ModuleLoadFailed) =>
        new ModuleLoadSuccess(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing, a.result)
      case (a: ModuleLoadFailed, b: ModuleLoadFailed) => new ModuleLoadFailed(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing)
    }
  }

  private[this] def wrapper[Modules, Pivot](
      extract: String => Pivot
  )(func: Pivot => ModuleLoadResult[Modules])(implicit ev1: Monoid[ModuleLoadResult[Modules]], ev2: Semigroup[Pivot]): Set[String] => Return[Modules] = {
    modules =>
      NonEmptyList
        .fromList(modules.toList.sorted)
        .fold(Monoid[ModuleLoadResult[Modules]].empty) { nel =>
          func(nel.map(extract).reduce)
        }
        .toOption
  }

  private[this] def work[A](a: Seq[String => Option[A]]): String => ModuleLoadResult[A] = { module =>
    a.foldLeft[ModuleLoadResult[A]](new ModuleLoadFailed(Set(module), Set.empty, Set.empty)) { case (acc, next) =>
      acc.combine(next(module).fold[ModuleLoadResult[A]](new ModuleLoadFailed(Set(module), Set.empty, Set.empty)) { res =>
        new ModuleLoadSuccess[A](Set.empty, Set(module), Set.empty, res)
      })
    }
  }

  type Return[A] = Option[A]

  def forProduct1[A, Z](a: Seq[String => Option[A]])(combine: A => Z): Set[String] => Return[Z] =
    wrapper(module => work[A](a)(module))(_.map(combine))
  def forProduct2[A, B, Z](a: Seq[String => Option[A]], b: Seq[String => Option[B]])(combine: (A, B) => Z): Set[String] => Return[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module)))(_.mapN(combine))
  def forProduct3[A, B, C, Z](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]]
  )(combine: (A, B, C) => Z): Set[String] => Return[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module)))(_.mapN(combine))
  def forProduct4[A, B, C, D, Z](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]],
      d: Seq[String => Option[D]]
  )(combine: (A, B, C, D) => Z): Set[String] => Return[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module), work[D](d)(module)))(_.mapN(combine))
  def forProduct5[A, B, C, D, E, Z](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]],
      d: Seq[String => Option[D]],
      e: Seq[String => Option[E]]
  )(combine: (A, B, C, D, E) => Z): Set[String] => Return[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module), work[D](d)(module), work[E](e)(module)))(_.mapN(combine))
  def forProduct6[A, B, C, D, E, F, Z](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]],
      d: Seq[String => Option[D]],
      e: Seq[String => Option[E]],
      f: Seq[String => Option[F]]
  )(combine: (A, B, C, D, E, F) => Z): Set[String] => Return[Z] =
    wrapper(module =>
      (
        work[A](a)(module),
        work[B](b)(module),
        work[C](c)(module),
        work[D](d)(module),
        work[E](e)(module),
        work[F](f)(module)
      )
    )(_.mapN(combine))

  def buildFrom[Modules, Extracted[_]](extract: String => Extracted[Modules])(implicit ev1: Monoid[Extracted[Modules]]): Set[String] => Extracted[Modules] = {
    parameters =>
      NonEmptyList
        .fromList(parameters.toList)
        .fold(Monoid[Extracted[Modules]].empty) { nel =>
          nel.map(extract).reduce
        }
  }
}

sealed abstract class ModuleLoadResult[+A](val attempted: Set[String], val consumed: Set[String], val missing: Set[String]) {
  def map[B](f: A => B): ModuleLoadResult[B]
  def flatMap[B](f: A => ModuleLoadResult[B]): ModuleLoadResult[B]
  def toOption: Option[A]
}

class ModuleLoadFailed(attempted: Set[String], consumed: Set[String], missing: Set[String]) extends ModuleLoadResult[Nothing](attempted, consumed, missing) {
  def map[B](f: Nothing => B)                       = this
  def flatMap[B](f: Nothing => ModuleLoadResult[B]) = this
  def toOption: Option[Nothing]                     = None
  override def toString(): String                   = s"ModuleLoadFailed($attempted, $consumed, $missing)"
}

class ModuleLoadSuccess[A](attempted: Set[String], consumed: Set[String], missing: Set[String], val result: A)
    extends ModuleLoadResult[A](attempted, consumed, missing) {
  def map[B](f: A => B) = new ModuleLoadSuccess[B](attempted, consumed, missing, f(result))
  def flatMap[B](f: A => ModuleLoadResult[B]) =
    f(result) match {
      case next: ModuleLoadFailed     => new ModuleLoadFailed(attempted ++ next.attempted, consumed ++ next.consumed, missing ++ next.missing)
      case next: ModuleLoadSuccess[_] => new ModuleLoadSuccess[B](attempted ++ next.attempted, consumed ++ next.consumed, missing ++ next.missing, next.result)
    }
  def toOption: Option[A]         = Some(result)
  override def toString(): String = s"ModuleLoadSuccess($attempted, $consumed, $missing, $result)"
}
