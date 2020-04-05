package com.twilio.guardrail

import com.twilio.swagger.core.StructuredLogger
import cats.implicits._
import cats.{ Applicative, Eval, MonadError, Traverse }

object CoreTarget extends LogAbstraction {
  implicit val coreTargetInstances = new MonadError[CoreTarget, Error] with Traverse[CoreTarget] {
    def pure[A](x: A): CoreTarget[A] = new CoreTargetValue(x)

    def handleErrorWith[A](fa: CoreTarget[A])(f: com.twilio.guardrail.Error => CoreTarget[A]): CoreTarget[A] = fa match {
      case ct @ CoreTargetValue(_) => ct
      case CoreTargetError(err)    => f(err)
    }
    def raiseError[A](e: com.twilio.guardrail.Error): CoreTarget[A] = new CoreTargetError(e)

    def flatMap[A, B](fa: CoreTarget[A])(f: A => CoreTarget[B]): CoreTarget[B] = fa match {
      case CoreTargetValue(a)        => f(a)
      case ct @ CoreTargetError(err) => new CoreTargetError(err)
    }
    def tailRecM[A, B](a: A)(f: A => CoreTarget[Either[A, B]]): CoreTarget[B] =
      f(a) match {
        case CoreTargetError(err) =>
          new CoreTargetError(err)
        case CoreTargetValue(e) =>
          e match {
            case Left(b)  => tailRecM(b)(f)
            case Right(a) => new CoreTargetValue(a)
          }
      }

    def foldLeft[A, B](fa: CoreTarget[A], b: B)(f: (B, A) => B): B = fa match {
      case CoreTargetValue(a) => f(b, a)
      case CoreTargetError(_) => b
    }

    def foldRight[A, B](fa: CoreTarget[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case CoreTargetValue(a) => f(a, lb)
      case CoreTargetError(_) => lb
    }

    def traverse[G[_], A, B](fa: CoreTarget[A])(f: A => G[B])(implicit G: Applicative[G]): G[CoreTarget[B]] = fa match {
      case CoreTargetValue(a) => G.ap(G.pure(CoreTarget.pure[B](_)))(f(a))
      case CoreTargetError(e) => G.pure[CoreTarget[B]](new CoreTargetError(e))
    }
  }

  type F[A] = CoreTarget[A]
  val A                                                     = Applicative[CoreTarget]
  def pushLogger(value: StructuredLogger): CoreTarget[Unit] = new CoreTargetValue(()) // IndexedStateT.modify(_ |+| value))
  def pure[T](x: T): CoreTarget[T]                          = x.pure[CoreTarget]
  def fromOption[T](x: Option[T], default: => Error): CoreTarget[T] =
    x.fold[CoreTarget[T]](new CoreTargetError(default))(new CoreTargetValue[T](_))
  @deprecated("Use raiseError instead", "v0.41.2")
  def error[T](x: Error): CoreTarget[T]      = new CoreTargetError(x)
  def raiseError[T](x: Error): CoreTarget[T] = new CoreTargetError(x)
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeExtract[T](x: CoreTarget[T]): T =
    x.valueOr({ err =>
      throw new Exception(err.toString)
    })
  //.runEmptyA
}

sealed abstract class CoreTarget[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA
  def fold[B](fail: Error => B, pass: A => B): B
  def leftFlatMap[AA >: A](f: Error => CoreTarget[AA]): CoreTarget[AA]
}

object CoreTargetValue {
  def unapply[A](x: CoreTargetValue[A]): Option[A] = Some(x.value)
}
class CoreTargetValue[A](val value: A) extends CoreTarget[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA                      = value
  def fold[B](fail: Error => B, pass: A => B): B                       = pass(value)
  def leftFlatMap[AA >: A](f: Error => CoreTarget[AA]): CoreTarget[AA] = new CoreTargetValue(value)
}
object CoreTargetError {
  def unapply[A](x: CoreTargetError[A]): Option[Error] = Some(x.error)
}
class CoreTargetError[A](val error: Error) extends CoreTarget[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA                      = fallback(error)
  def fold[B](fail: Error => B, pass: A => B): B                       = fail(error)
  def leftFlatMap[AA >: A](f: Error => CoreTarget[AA]): CoreTarget[AA] = f(error)
}
