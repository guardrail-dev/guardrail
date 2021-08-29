package dev.guardrail.terms.collections

import dev.guardrail.languages.LA
import dev.guardrail.terms.collections.TermHolder.StringMap
import java.util.concurrent.CompletionStage
import scala.concurrent.Future
import scala.reflect.ClassTag

class TermHolder[L <: LA, +A, HeldType](_value: A) {
  def value: A = _value

  override def equals(obj: Any): Boolean = Option(obj) match {
    case Some(other: TermHolder[_, _, _]) => value.equals(other.value)
    case _                                => false
  }
  override def hashCode(): Int  = value.hashCode()
  override def toString: String = value.toString
}

object TermHolder {
  type StringMap[A] = Map[String, A]

  def apply[L <: LA, A, HeldType](value: A): TermHolder[L, A, HeldType] = new TermHolder[L, A, HeldType](value)
  def unapply[A](th: TermHolder[_, A, _]): Option[A]                    = Some(th.value)

  final private[collections] class TermHolderPartiallyApplied[L <: LA, HeldType] private[TermHolder] (private val dummy: Boolean = true) extends AnyVal {
    def apply[A <: L#Expression](fa: A): TermHolder[L, A, HeldType] = TermHolder[L, A, HeldType](fa)
  }

  def lift[L <: LA, HeldType]: TermHolderPartiallyApplied[L, HeldType] = new TermHolderPartiallyApplied[L, HeldType]
}

trait MonadF[L <: LA, F[_]] {
  def liftType(tpe: L#Type): L#Type
  def isType(tpe: L#Type): Boolean
  def pure[From <: L#Expression, A](fa: TermHolder[L, From, A]): TermHolder[L, L#Apply, F[A]]
  def filter[From <: L#Expression, A, Func <: L#Expression](f: TermHolder[L, Func, A => Boolean])(fa: TermHolder[L, From, F[A]])(
      implicit clsA: ClassTag[A]
  ): TermHolder[L, L#Apply, F[A]]
  def foreach[From <: L#Expression, A, Func <: L#Expression](f: TermHolder[L, Func, A => Unit])(fa: TermHolder[L, From, F[A]]): TermHolder[L, L#Apply, Unit]
  def map[From <: L#Expression, A, B, Func <: L#Expression](f: TermHolder[L, Func, A => B])(fa: TermHolder[L, From, F[A]]): TermHolder[L, L#Apply, F[B]]
  def flatMap[From <: L#Expression, A, B, Func <: L#Expression](f: TermHolder[L, Func, A => F[B]])(fa: TermHolder[L, From, F[A]]): TermHolder[L, L#Apply, F[B]]
}

trait MonadFSyntax[L <: LA] {
  implicit class TermHolderSyntaxMonadF[From <: L#Expression, F[_], A: ClassTag](fa: TermHolder[L, From, F[A]])(implicit ev: MonadF[L, F]) {
    def filter[Func <: L#Expression](f: TermHolder[L, Func, A => Boolean]): TermHolder[L, L#Apply, F[A]]  = ev.filter(f)(fa)
    def foreach[Func <: L#Expression](f: TermHolder[L, Func, A => Unit]): TermHolder[L, L#Apply, Unit]    = ev.foreach(f)(fa)
    def map[Func <: L#Expression, B](f: TermHolder[L, Func, A => B]): TermHolder[L, L#Apply, F[B]]        = ev.map(f)(fa)
    def flatMap[Func <: L#Expression, B](f: TermHolder[L, Func, A => F[B]]): TermHolder[L, L#Apply, F[B]] = ev.flatMap(f)(fa)
  }

  implicit class TermHolderExpressionSyntaxMonadF[From <: L#Expression, A](fa: TermHolder[L, From, A]) {
    def liftOptional(implicit ev: MonadF[L, Option]): TermHolder[L, L#Apply, Option[A]]  = ev.pure(fa)
    def liftList(implicit ev: MonadF[L, List]): TermHolder[L, L#Apply, List[A]]          = ev.pure(fa)
    def liftMap(implicit ev: MonadF[L, StringMap]): TermHolder[L, L#Apply, StringMap[A]] = ev.pure(fa)
    def liftFuture(implicit ev: MonadF[L, Future]): TermHolder[L, L#Apply, Future[A]]    = ev.pure(fa)
  }
}

trait ToArrayF[L <: LA, F[_]] {
  def toArray[From <: L#Expression, A](fa: TermHolder[L, From, F[A]])(implicit clsA: ClassTag[A]): TermHolder[L, L#Apply, Array[A]]
}

trait ToArrayFSyntax[L <: LA] {
  implicit class TermHolderSyntaxToArrayF[From <: L#Expression, F[_], A: ClassTag](fa: TermHolder[L, From, F[A]])(implicit ev: ToArrayF[L, F]) {
    def toArray: TermHolder[L, L#Apply, Array[A]] = ev.toArray(fa)
  }
}

trait EmptyF[L <: LA, F[_]] {
  def empty[A]: TermHolder[L, L#Apply, F[A]]
}

trait EmptyFBuilders[L <: LA] {
  def emptyOptional[A](implicit ev: EmptyF[L, Option]): TermHolder[L, L#Apply, Option[A]]  = ev.empty
  def emptyList[A](implicit ev: EmptyF[L, List]): TermHolder[L, L#Apply, List[A]]          = ev.empty
  def emptyMap[A](implicit ev: EmptyF[L, StringMap]): TermHolder[L, L#Apply, StringMap[A]] = ev.empty
}

trait ListF[L <: LA] extends MonadF[L, List] with EmptyF[L, List] with ToArrayF[L, List]

trait ListFSyntax[L <: LA] {
  implicit class ListTypeSyntaxMonadF(tpe: L#Type)(implicit ev: ListF[L]) {
    def liftListType: L#Type = ev.liftType(tpe)
    def isListType: Boolean  = ev.isType(tpe)
  }
}

trait OptionF[L <: LA] extends MonadF[L, Option] with EmptyF[L, Option] {
  def getOrElse[From <: L#Expression, A, B >: A, Func <: L#Expression](f: TermHolder[L, Func, () => B])(
      fa: TermHolder[L, From, Option[A]]
  ): TermHolder[L, L#Apply, B]

  def getOrElseNull[From <: L#Expression, A](fa: TermHolder[L, From, Option[A]]): TermHolder[L, L#Apply, A]

  def getOrElseThrow[From <: L#Expression, A, X <: Throwable, Func <: L#Expression](f: TermHolder[L, Func, () => X])(
      fa: TermHolder[L, From, Option[A]]
  ): TermHolder[L, L#Apply, A]
}

trait OptionFSyntax[L <: LA] {
  implicit class TermHolderSyntaxOptionF[From <: L#Expression, A](fa: TermHolder[L, From, Option[A]])(implicit ev: OptionF[L]) {
    def getOrElse[Func <: L#Expression, B >: A](f: TermHolder[L, Func, () => B]): TermHolder[L, L#Apply, B]              = ev.getOrElse(f)(fa)
    def getOrElseNull: TermHolder[L, L#Apply, A]                                                                         = ev.getOrElseNull(fa)
    def getOrElseThrow[Func <: L#Expression, X <: Throwable](f: TermHolder[L, Func, () => X]): TermHolder[L, L#Apply, A] = ev.getOrElseThrow(f)(fa)
  }

  implicit class OptionTypeSyntaxMonadF(tpe: L#Type)(implicit ev: OptionF[L]) {
    def liftOptionalType: L#Type = ev.liftType(tpe)
    def isOptionalType: Boolean  = ev.isType(tpe)
  }
}

trait FutureF[L <: LA] extends MonadF[L, Future] {
  def fromCompletionStage[From <: L#Expression, A](fa: TermHolder[L, From, CompletionStage[A]]): TermHolder[L, L#Expression, Future[A]]
  def toCompletionStage[From <: L#Expression, A](fa: TermHolder[L, From, Future[A]]): TermHolder[L, L#Expression, CompletionStage[A]]
  def failedFuture[From <: L#Expression, A, X <: Throwable](fx: TermHolder[L, From, X])(implicit clsA: ClassTag[A]): TermHolder[L, L#Expression, Future[A]]

  def onComplete[From <: L#Expression, A, X <: Throwable, Func <: L#Expression](fs: TermHolder[L, Func, A => Unit], ff: TermHolder[L, Func, X => Unit])(
      fa: TermHolder[L, From, Future[A]]
  ): TermHolder[L, L#Apply, Unit]
}

trait FutureFSyntax[L <: LA] {
  implicit class TermHolderSyntaxFutureF[From <: L#Expression, A](fa: TermHolder[L, From, Future[A]])(implicit ev: FutureF[L]) {
    def toCompletionStage: TermHolder[L, L#Expression, CompletionStage[A]] = ev.toCompletionStage(fa)
    def onComplete[X <: Throwable, Func <: L#Expression](fs: TermHolder[L, Func, A => Unit], ff: TermHolder[L, Func, X => Unit]): TermHolder[L, L#Apply, Unit] =
      ev.onComplete(fs, ff)(fa)
  }

  implicit class TermHolderSyntaxCompletionStageF[From <: L#Expression, A](fa: TermHolder[L, From, CompletionStage[A]])(implicit ev: FutureF[L]) {
    def toFuture: TermHolder[L, L#Expression, Future[A]] = ev.fromCompletionStage(fa)
  }

  implicit class TermHolderExpresionSyntaxFutureF[From <: L#Expression, X <: Throwable](fx: TermHolder[L, From, X])(implicit ev: FutureF[L]) {
    def toFailedFuture[A: ClassTag]: TermHolder[L, L#Expression, Future[A]] = ev.failedFuture[From, A, X](fx)
  }

  implicit class FutureTypeSyntaxMonadF(tpe: L#Type)(implicit ev: FutureF[L]) {
    def liftFutureType: L#Type = ev.liftType(tpe)
    def isFutureType: Boolean  = ev.isType(tpe)
  }
}

trait CollectionsAbstractionSyntax[L <: LA]
    extends MonadFSyntax[L]
    with ToArrayFSyntax[L]
    with ListFSyntax[L]
    with OptionFSyntax[L]
    with FutureFSyntax[L]
    with EmptyFBuilders[L] {
  implicit class ExpressionLiftSyntax[From <: L#Expression](a: From) {
    def lift[HeldType]: TermHolder[L, From, HeldType] = TermHolder.lift[L, HeldType](a)
  }
}

trait CollectionsAbstraction[L <: LA] extends CollectionsAbstractionSyntax[L] {
  implicit def optionInstances: OptionF[L]
  implicit def listInstances: ListF[L]
  implicit def futureInstances: FutureF[L]

  def copy(
      newOptionInstances: OptionF[L] = optionInstances,
      newListInstances: ListF[L] = listInstances,
      newFutureInstances: FutureF[L] = futureInstances
  ): CollectionsAbstraction[L] = new CollectionsAbstraction[L] {
    override implicit def optionInstances: OptionF[L] = newOptionInstances
    override implicit def listInstances: ListF[L]     = newListInstances
    override implicit def futureInstances: FutureF[L] = newFutureInstances
  }
}
