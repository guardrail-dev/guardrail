package com.twilio.guardrail

import com.twilio.swagger.core.StructuredLogger
import cats.{ Applicative, MonadError }
import cats.data.EitherT
import cats.Traverse
import cats.Eval

object Target extends LogAbstraction {
  type F[A] = Target[A]
  val A = new Applicative[Target] {
    def pure[A](x: A): Target[A] = new TargetValue(x)
    def ap[A, B](ff: Target[A => B])(fa: Target[A]): Target[B] =
      (ff, fa) match {
        case (TargetValue(f), TargetValue(a)) => new TargetValue(f(a))
        case (TargetError(err), _)            => new TargetError(err)
        case (_, TargetError(err))            => new TargetError(err)
      }
  }
  def pushLogger(value: StructuredLogger): Target[Unit] = new TargetValue(()) // IndexedStateT.modify(_ |+| value))
  def pure[T](x: T): Target[T]                          = A.pure(x)
  @deprecated("Use raiseError instead", "v0.41.2")
  def error[T](x: String): Target[T]          = raiseError(x)
  def raiseError[T](x: String): Target[T]     = new TargetError(UserError(x))
  def raiseException[T](x: String): Target[T] = new TargetError(RuntimeFailure(x))
  def fromOption[T](x: Option[T], default: => String): Target[T] =
    x.fold[Target[T]](new TargetError(UserError(default)))(new TargetValue(_))
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeExtract[T](x: Target[T]): T =
    x.valueOr({ err =>
      throw new Exception(err.toString)
    })
  // .runEmptyA

  implicit val targetInstances = new MonadError[Target, Error] with Traverse[Target] {
    def pure[A](x: A): Target[A] = Target.A.pure(x)

    def handleErrorWith[A](fa: Target[A])(f: Error => Target[A]): Target[A] = fa match {
      case fa @ TargetValue(_) => fa
      case TargetError(err)    => f(err)
    }
    def raiseError[A](e: Error): Target[A] = new TargetError(e)

    def flatMap[A, B](fa: Target[A])(f: A => Target[B]): Target[B] = fa match {
      case TargetValue(a)   => f(a)
      case TargetError(err) => new TargetError(err)
    }
    def tailRecM[A, B](a: A)(f: A => Target[Either[A, B]]): Target[B] =
      f(a) match {
        case TargetError(err) =>
          new TargetError(err)
        case TargetValue(e) =>
          e match {
            case Left(b)  => tailRecM(b)(f)
            case Right(a) => new TargetValue(a)
          }
      }

    def foldLeft[A, B](fa: Target[A], b: B)(f: (B, A) => B): B = fa match {
      case TargetValue(a) => f(b, a)
      case TargetError(_) => b
    }
    def foldRight[A, B](fa: Target[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case TargetValue(a) => f(a, lb)
      case TargetError(_) => lb
    }
    def traverse[G[_], A, B](fa: Target[A])(f: A => G[B])(implicit G: Applicative[G]): G[Target[B]] = fa match {
      case TargetValue(a) => G.ap(G.pure(Target.pure[B](_)))(f(a))
      case TargetError(e) => G.pure[Target[B]](new TargetError(e))
    }
  }
}

sealed abstract class Target[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA
  def toEitherT: EitherT[cats.Id, Error, A]
  def map[B](f: A => B): Target[B]
  def flatMap[B](f: A => Target[B]): Target[B]
  def recover[AA >: A](f: Error => AA): Target[AA]
}
object TargetValue {
  def unapply[A](x: TargetValue[A]): Option[A] = Some(x.value)
}
class TargetValue[A](val value: A) extends Target[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA  = value
  def toEitherT: EitherT[cats.Id, Error, A]        = EitherT.right[Error](cats.Monad[cats.Id].pure(value))
  def map[B](f: A => B): Target[B]                 = new TargetValue(f(value))
  def flatMap[B](f: A => Target[B]): Target[B]     = f(value)
  def recover[AA >: A](f: Error => AA): Target[AA] = new TargetValue(value)
}
object TargetError {
  def unapply[A](x: TargetError[A]): Option[Error] = Some(x.error)
}
class TargetError[A](val error: Error) extends Target[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA  = fallback(error)
  def toEitherT: EitherT[cats.Id, Error, A]        = EitherT.left[A](cats.Monad[cats.Id].pure(error))
  def map[B](f: A => B): Target[B]                 = new TargetError(error)
  def flatMap[B](f: A => Target[B]): Target[B]     = new TargetError(error)
  def recover[AA >: A](f: Error => AA): Target[AA] = new TargetValue(f(error))
}
