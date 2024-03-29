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
          case a: ModuleLoadSuccess[A] =>
            new ModuleLoadSuccess[B](f.modulesConsumed.combine(a.modulesConsumed), f.componentsConsumed.combine(a.componentsConsumed), f.result(a.result))
          case fail: ModuleLoadFailed =>
            ModuleLoadFailed.of(fail.attempted, fail.componentsConsumed.combine(f.componentsConsumed), fail.choices)
          case conflict: ModuleLoadConflict => conflict
        }
      case ffail: ModuleLoadFailed =>
        fa match {
          case succ: ModuleLoadSuccess[_] =>
            ModuleLoadFailed.of(ffail.attempted, ffail.componentsConsumed.combine(succ.componentsConsumed), ffail.choices)
          case afail: ModuleLoadFailed =>
            ModuleLoadFailed.of(
              ffail.attempted.combine(afail.attempted),
              ffail.componentsConsumed.combine(afail.componentsConsumed),
              ffail.choices.combine(afail.choices)
            )
          case conflict: ModuleLoadConflict => conflict
        }
      case conflict: ModuleLoadConflict => conflict
    }
  }

  implicit def moduleLoadResultSemigroup[A]: Monoid[ModuleLoadResult[A]] = new Monoid[ModuleLoadResult[A]] {
    def empty: ModuleLoadResult[A] = ModuleLoadFailed.of(Set.empty, Set.empty, Map.empty)
    def combine(a: ModuleLoadResult[A], b: ModuleLoadResult[A]): ModuleLoadResult[A] =
      (a, b) match {
        case (a: ModuleLoadConflict, _) =>
          a
        case (_, b: ModuleLoadConflict) =>
          b
        case (a: ModuleLoadSuccess[A], b: ModuleLoadSuccess[A]) =>
          a.componentsConsumed.intersect(b.componentsConsumed).toSeq match {
            case Seq(only) => new ModuleLoadConflict(only)
            case other     => new ModuleLoadConflict(other.mkString(", "))
          }
        case (a: ModuleLoadFailed, b: ModuleLoadSuccess[A]) =>
          b
        case (a: ModuleLoadSuccess[A], b: ModuleLoadFailed) =>
          a
        case (a: ModuleLoadFailed, b: ModuleLoadFailed) =>
          ModuleLoadFailed.of(a.attempted.combine(b.attempted), a.componentsConsumed.combine(b.componentsConsumed), a.choices.combine(b.choices))
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

  type ModuleDescriptor[A] = (String, Seq[Map[String, A]])

  private[this] def work[A](a: ModuleDescriptor[A]): String => ModuleLoadResult[A] = { module =>
    val (label, mappings) = a
    val choices           = Map(label -> mappings.flatMap(_.keys).toSet)
    mappings.foldLeft[ModuleLoadResult[A]](ModuleLoadFailed.of(Set.empty, Set.empty, choices)) { case (acc, next) =>
      acc.combine(
        next
          .get(module)
          .fold[ModuleLoadResult[A]](ModuleLoadFailed.of(Set(module), Set.empty, Map.empty)) { res =>
            new ModuleLoadSuccess[A](Set(module), Set(label), res)
          }
      )
    }
  }

  private[this] def extractLabel[A](descriptor: ModuleDescriptor[A]): Set[String] = Set(descriptor._1)
  private[this] def extractChoices[A](descriptor: ModuleDescriptor[A]): Map[String, Set[String]] = {
    val (label, mappings) = descriptor
    Map(label -> mappings.flatMap(_.keys).toSet)
  }

  def emitDefault[A](a: A): Set[String] => ModuleLoadResult[A] = _ => new ModuleLoadSuccess[A](Set.empty, Set.empty, a)
  def forProduct1[A, Z](a: ModuleDescriptor[A])(combine: A => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => work[A](a)(module))(_.map(combine)).map(_.getOrElse(ModuleLoadFailed.of(Set.empty, Set.empty, extractChoices(a))))
  def forProduct2[A, B, Z](a: ModuleDescriptor[A], b: ModuleDescriptor[B])(combine: (A, B) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module)))(_.mapN(combine))
      .map(_.getOrElse(ModuleLoadFailed.of(Set.empty, Set.empty, extractChoices(a).combine(extractChoices(b)))))
  def forProduct3[A, B, C, Z](
      a: ModuleDescriptor[A],
      b: ModuleDescriptor[B],
      c: ModuleDescriptor[C]
  )(combine: (A, B, C) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module)))(_.mapN(combine))
      .map(
        _.getOrElse(
          ModuleLoadFailed.of(
            Set.empty,
            Set.empty,
            extractChoices(a).combine(extractChoices(b)).combine(extractChoices(c))
          )
        )
      )
  def forProduct4[A, B, C, D, Z](
      a: ModuleDescriptor[A],
      b: ModuleDescriptor[B],
      c: ModuleDescriptor[C],
      d: ModuleDescriptor[D]
  )(combine: (A, B, C, D) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module), work[D](d)(module)))(_.mapN(combine))
      .map(
        _.getOrElse(
          ModuleLoadFailed.of(
            Set.empty,
            Set.empty,
            extractChoices(a).combine(extractChoices(b)).combine(extractChoices(c)).combine(extractChoices(d))
          )
        )
      )
  def forProduct5[A, B, C, D, E, Z](
      a: ModuleDescriptor[A],
      b: ModuleDescriptor[B],
      c: ModuleDescriptor[C],
      d: ModuleDescriptor[D],
      e: ModuleDescriptor[E]
  )(combine: (A, B, C, D, E) => Z): Set[String] => ModuleLoadResult[Z] =
    wrapper(module => (work[A](a)(module), work[B](b)(module), work[C](c)(module), work[D](d)(module), work[E](e)(module)))(_.mapN(combine))
      .map(
        _.getOrElse(
          ModuleLoadFailed.of(
            Set.empty,
            Set.empty,
            extractChoices(a).combine(extractChoices(b)).combine(extractChoices(c)).combine(extractChoices(d)).combine(extractChoices(e))
          )
        )
      )
  def forProduct6[A, B, C, D, E, F, Z](
      a: ModuleDescriptor[A],
      b: ModuleDescriptor[B],
      c: ModuleDescriptor[C],
      d: ModuleDescriptor[D],
      e: ModuleDescriptor[E],
      f: ModuleDescriptor[F]
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
    )(_.mapN(combine)).map(
      _.getOrElse(
        ModuleLoadFailed.of(
          Set.empty,
          Set.empty,
          extractChoices(a)
            .combine(extractChoices(b))
            .combine(extractChoices(c))
            .combine(extractChoices(d))
            .combine(extractChoices(e))
            .combine(extractChoices(f))
        )
      )
    )

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
sealed abstract class ModuleLoadResult[+A]

class ModuleLoadConflict(val section: String) extends ModuleLoadResult[Nothing] {
  override def toString(): String = s"ModuleLoadConflict($section)"
}

class ModuleLoadFailed private[ModuleLoadFailed] (val attempted: Set[String], val componentsConsumed: Set[String], val choices: Map[String, Set[String]])
    extends ModuleLoadResult[Nothing] {
  override def toString(): String = s"ModuleLoadFailed($attempted, $componentsConsumed, $choices)"
}

object ModuleLoadFailed {
  def of(attempted: Set[String], componentsConsumed: Set[String], choices: Map[String, Set[String]]): ModuleLoadFailed = {
    val filteredChoices = choices.toSeq.filterNot { case (k, _) => componentsConsumed.contains(k) }.toMap
    new ModuleLoadFailed(attempted, componentsConsumed, filteredChoices)
  }
}

class ModuleLoadSuccess[A](val modulesConsumed: Set[String], val componentsConsumed: Set[String], val result: A) extends ModuleLoadResult[A] {
  override def toString(): String = s"ModuleLoadSuccess($modulesConsumed, $componentsConsumed, $result)"
}
