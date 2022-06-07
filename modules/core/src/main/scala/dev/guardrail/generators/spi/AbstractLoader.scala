package dev.guardrail.generators.spi

import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._
import dev.guardrail.languages.LA
import dev.guardrail.{ MissingDependency, Target }
import java.util.ServiceLoader
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object ModuleLoadResult {
  val empty: ModuleLoadResult[Nothing] = new ModuleLoadFailed(Set.empty, Set.empty, Set.empty)

  def extract[A](f: String => Option[A]): String => ModuleLoadResult[A] = { module =>
    f(module).fold[ModuleLoadResult[A]](new ModuleLoadFailed(Set(module), Set.empty, Set.empty)) { res =>
      new ModuleLoadSuccess[A](Set.empty, Set(module), Set.empty, res)
    }
  }

  implicit def moduleLoadResultSemigroup[A]: Semigroup[ModuleLoadResult[A]] = {
    case (a: ModuleLoadSuccess[A], b: ModuleLoadSuccess[A]) =>
      new ModuleLoadSuccess(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing, a.result)
    case (a: ModuleLoadFailed, b: ModuleLoadSuccess[A]) =>
      new ModuleLoadSuccess(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing, b.result)
    case (a: ModuleLoadSuccess[A], b: ModuleLoadFailed) =>
      new ModuleLoadSuccess(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing, a.result)
    case (a: ModuleLoadFailed, b: ModuleLoadFailed) => new ModuleLoadFailed(a.attempted ++ b.attempted, a.consumed ++ b.consumed, a.missing ++ b.missing)
  }

  def forProduct1[A](a: Seq[String => Option[A]]): String => ModuleLoadResult[A] = { module =>
    a.foldLeft[ModuleLoadResult[A]](new ModuleLoadFailed(Set(module), Set.empty, Set.empty)) { case (acc, next) =>
      acc.combine(ModuleLoadResult.extract(next)(module))
    }
  }
  def forProduct2[A, B](a: Seq[String => Option[A]], b: Seq[String => Option[B]]): String => ModuleLoadResult[(A, B)] = { module =>
    val (resA, resB) = (forProduct1[A](a)(module), forProduct1[B](b)(module))
    for {
      a <- resA
      b <- resB
    } yield (a, b)
  }
  def forProduct3[A, B, C](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]]
  ): String => ModuleLoadResult[(A, B, C)] = { module =>
    val (resA, resB, resC) = (forProduct1[A](a)(module), forProduct1[B](b)(module), forProduct1[C](c)(module))
    for {
      a <- resA
      b <- resB
      c <- resC
    } yield (a, b, c)
  }
  def forProduct4[A, B, C, D](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]],
      d: Seq[String => Option[D]]
  ): String => ModuleLoadResult[(A, B, C, D)] = { module =>
    val (resA, resB, resC, resD) = (forProduct1[A](a)(module), forProduct1[B](b)(module), forProduct1[C](c)(module), forProduct1[D](d)(module))
    for {
      a <- resA
      b <- resB
      c <- resC
      d <- resD
    } yield (a, b, c, d)
  }
  def forProduct5[A, B, C, D, E](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]],
      d: Seq[String => Option[D]],
      e: Seq[String => Option[E]]
  ): String => ModuleLoadResult[(A, B, C, D, E)] = { module =>
    val (resA, resB, resC, resD, resE) =
      (forProduct1[A](a)(module), forProduct1[B](b)(module), forProduct1[C](c)(module), forProduct1[D](d)(module), forProduct1[E](e)(module))
    for {
      a <- resA
      b <- resB
      c <- resC
      d <- resD
      e <- resE
    } yield (a, b, c, d, e)
  }
  def forProduct6[A, B, C, D, E, F](
      a: Seq[String => Option[A]],
      b: Seq[String => Option[B]],
      c: Seq[String => Option[C]],
      d: Seq[String => Option[D]],
      e: Seq[String => Option[E]],
      f: Seq[String => Option[F]]
  ): String => ModuleLoadResult[(A, B, C, D, E, F)] = { module =>
    val (resA, resB, resC, resD, resE, resF) = (
      forProduct1[A](a)(module),
      forProduct1[B](b)(module),
      forProduct1[C](c)(module),
      forProduct1[D](d)(module),
      forProduct1[E](e)(module),
      forProduct1[F](f)(module)
    )
    for {
      a <- resA
      b <- resB
      c <- resC
      d <- resD
      e <- resE
      f <- resF
    } yield (a, b, c, d, e, f)
  }

  def buildFrom[Modules, Result](extract: String => ModuleLoadResult[Modules])(build: Modules => Result): Set[String] => Option[Result] = { parameters =>
    NonEmptyList
      .fromList(parameters.toList)
      .flatMap(_.map(extract).reduce match {
        case res: ModuleLoadSuccess[Modules] => Some(build(res.result))
        case fail: ModuleLoadFailed          => None
      })
  }
}

sealed abstract class ModuleLoadResult[+A](val attempted: Set[String], val consumed: Set[String], val missing: Set[String]) {
  def map[B](f: A => B): ModuleLoadResult[B]
  def flatMap[B](f: A => ModuleLoadResult[B]): ModuleLoadResult[B]
}

class ModuleLoadFailed(attempted: Set[String], consumed: Set[String], missing: Set[String]) extends ModuleLoadResult[Nothing](attempted, consumed, missing) {
  def map[B](f: Nothing => B)                       = this
  def flatMap[B](f: Nothing => ModuleLoadResult[B]) = this
  override def toString(): String = s"ModuleLoadFailed($attempted, $consumed, $missing)"
}

class ModuleLoadSuccess[A](attempted: Set[String], consumed: Set[String], missing: Set[String], val result: A)
    extends ModuleLoadResult[A](attempted, consumed, missing) {
  def map[B](f: A => B) = new ModuleLoadSuccess[B](attempted, consumed, missing, f(result))
  def flatMap[B](f: A => ModuleLoadResult[B]) =
    f(result) match {
      case next: ModuleLoadFailed     => new ModuleLoadFailed(attempted ++ next.attempted, consumed ++ next.consumed, missing ++ next.missing)
      case next: ModuleLoadSuccess[_] => new ModuleLoadSuccess[B](attempted ++ next.attempted, consumed ++ next.consumed, missing ++ next.missing, next.result)
    }
  override def toString(): String = s"ModuleLoadSuccess($attempted, $consumed, $missing, $result)"
}

trait AbstractGeneratorLoader[A[_ <: LA, _[_]]] {
  type L <: LA
  def reified: TypeTag[Target[L]]
  def apply: Set[String] => Option[A[L, Target]]
}

abstract class AbstractGeneratorLoaderCompanion[A[_ <: LA, _[_]], B <: AbstractGeneratorLoader[A]](implicit ct: ClassTag[B]) {
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def loader: ServiceLoader[B] = ServiceLoader.load(ct.runtimeClass.asInstanceOf[Class[B]])
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def load[L <: LA](params: Set[String], error: MissingDependency)(implicit tt: TypeTag[Target[L]]): Target[A[L, Target]] = {
    val found = loader
      .iterator()
      .asScala
      .filter(_.reified.tpe =:= tt.tpe)
      .flatMap(_.apply(params).asInstanceOf[Option[A[L, Target]]])
      .toSeq
      .headOption

    Target.fromOption(found, error)
  }
}
