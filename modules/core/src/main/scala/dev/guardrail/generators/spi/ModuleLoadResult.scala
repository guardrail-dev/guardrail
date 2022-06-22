package dev.guardrail.generators.spi

import cats.Applicative
import cats.Monoid
import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._

object ModuleLoadResult {
  implicit val moduleLoadResultApplicative: Applicative[ModuleLoadResult] = new Applicative[ModuleLoadResult] {
    def pure[A](x: A): ModuleLoadResult[A] = new ModuleLoadSuccess(Set.empty, Set.empty, x)
    def ap[A, B](ff: ModuleLoadResult[A => B])(fa: ModuleLoadResult[A]): ModuleLoadResult[B] = ff match {
      case f: ModuleLoadSuccess[A => B] =>
        fa match {
          case a: ModuleLoadSuccess[A] => new ModuleLoadSuccess[B](ff.attempted ++ fa.attempted, f.consumed ++ a.consumed, f.result(a.result))
          case fail: ModuleLoadFailed  => fail
        }
      case ffail: ModuleLoadFailed =>
        fa match {
          case afail: ModuleLoadFailed => new ModuleLoadFailed(ffail.attempted ++ afail.attempted, ffail.missing ++ afail.missing)
          case _                       => ffail
        }
    }
  }

  implicit def moduleLoadResultSemigroup[A]: Monoid[ModuleLoadResult[A]] = new Monoid[ModuleLoadResult[A]] {
    def empty: ModuleLoadResult[A] = new ModuleLoadFailed(Set.empty, Set.empty)
    def combine(a: ModuleLoadResult[A], b: ModuleLoadResult[A]): ModuleLoadResult[A] =
      (a, b) match {
        case (a: ModuleLoadSuccess[A], b: ModuleLoadSuccess[A]) =>
          new ModuleLoadSuccess(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.result)
        case (a: ModuleLoadFailed, b: ModuleLoadSuccess[A]) =>
          new ModuleLoadSuccess(a.attempted ++ b.attempted, b.consumed, b.result)
        case (a: ModuleLoadSuccess[A], b: ModuleLoadFailed) =>
          new ModuleLoadSuccess(Set.empty, Set.empty, a.result)
        case (a: ModuleLoadFailed, b: ModuleLoadFailed) =>
          new ModuleLoadFailed(a.attempted ++ b.attempted, a.missing ++ b.missing)
      }
  }

  private[this] def wrapper[Modules, Pivot](
      extract: String => Pivot
  )(
      func: Pivot => ModuleLoadResult[Modules]
  )(implicit ev1: Monoid[ModuleLoadResult[Modules]], ev2: Semigroup[Pivot]): Set[String] => Option[ModuleLoadResult[Modules]] = { modules =>
    NonEmptyList
      .fromList(modules.toList.sorted)
      .map { nel =>
        func(nel.map(extract).reduce)
      }
  }

  private[this] def work[A](a: (String, Seq[Map[String, A]])): String => ModuleLoadResult[A] = { module =>
    val (label, mappings) = a
    mappings.foldLeft[ModuleLoadResult[A]](new ModuleLoadFailed(Set.empty, Set(label))) { case (acc, next) =>
      acc.combine(
        next
          .get(module)
          .fold[ModuleLoadResult[A]](new ModuleLoadFailed(Set(module), Set(label))) { res =>
            new ModuleLoadSuccess[A](Set(module), Set(module), res)
          }
      )
    }
  }

  def forProduct1[A, Z](a: (String, Seq[Map[String, A]]))(combine: A => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => work[A](a)(module))(_.map(combine)).map(_.getOrElse(new ModuleLoadFailed(Set.empty, Set(a._1))))
  def forProduct2[A, B, Z](a: (String, Seq[Map[String, A]]), b: (String, Seq[Map[String, B]]))(combine: (A, B) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module)))(_.mapN(combine)).map(_.getOrElse(new ModuleLoadFailed(Set.empty, Set(a._1, b._1))))
  def forProduct3[A, B, C, Z](
      a: (String, Seq[Map[String, A]]),
      b: (String, Seq[Map[String, B]]),
      c: (String, Seq[Map[String, C]])
  )(combine: (A, B, C) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module)))(_.mapN(combine))
      .map(_.getOrElse(new ModuleLoadFailed(Set.empty, Set(a._1, b._1, c._1))))
  def forProduct4[A, B, C, D, Z](
      a: (String, Seq[Map[String, A]]),
      b: (String, Seq[Map[String, B]]),
      c: (String, Seq[Map[String, C]]),
      d: (String, Seq[Map[String, D]])
  )(combine: (A, B, C, D) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module), work[D](d)(module)))(_.mapN(combine))
      .map(_.getOrElse(new ModuleLoadFailed(Set.empty, Set(a._1, b._1, c._1, d._1))))
  def forProduct5[A, B, C, D, E, Z](
      a: (String, Seq[Map[String, A]]),
      b: (String, Seq[Map[String, B]]),
      c: (String, Seq[Map[String, C]]),
      d: (String, Seq[Map[String, D]]),
      e: (String, Seq[Map[String, E]])
  )(combine: (A, B, C, D, E) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module), work[D](d)(module), work[E](e)(module)))(_.mapN(combine))
      .map(_.getOrElse(new ModuleLoadFailed(Set.empty, Set(a._1, b._1, c._1, d._1, e._1))))
  def forProduct6[A, B, C, D, E, F, Z](
      a: (String, Seq[Map[String, A]]),
      b: (String, Seq[Map[String, B]]),
      c: (String, Seq[Map[String, C]]),
      d: (String, Seq[Map[String, D]]),
      e: (String, Seq[Map[String, E]]),
      f: (String, Seq[Map[String, F]])
  )(combine: (A, B, C, D, E, F) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module =>
      (
        work[A](a)(module),
        work[B](b)(module),
        work[C](c)(module),
        work[D](d)(module),
        work[E](e)(module),
        work[F](f)(module)
      )
    )(_.mapN(combine)).map(_.getOrElse(new ModuleLoadFailed(Set.empty, Set(a._1, b._1, c._1, d._1, e._1, f._1))))

  def buildFrom[Modules, Extracted[_]](extract: String => Extracted[Modules])(implicit ev1: Monoid[Extracted[Modules]]): Set[String] => Extracted[Modules] = {
    parameters =>
      NonEmptyList
        .fromList(parameters.toList)
        .fold(Monoid[Extracted[Modules]].empty) { nel =>
          nel.map(extract).reduce
        }
  }
}

// Parameters:
// - attempted: All mapping keys for the current stage
sealed abstract class ModuleLoadResult[+A](val attempted: Set[String])

class ModuleLoadFailed(attempted: Set[String], val missing: Set[String]) extends ModuleLoadResult[Nothing](attempted) {
  override def toString(): String = s"ModuleLoadFailed($attempted, $missing)"
}

class ModuleLoadSuccess[A](attempted: Set[String], val consumed: Set[String], val result: A) extends ModuleLoadResult[A](attempted) {
  override def toString(): String = s"ModuleLoadSuccess($attempted, $consumed, $result)"
}
