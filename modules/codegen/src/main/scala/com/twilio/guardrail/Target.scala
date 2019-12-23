package com.twilio.guardrail

import com.twilio.swagger.core.StructuredLogger
import cats.{ Applicative, MonadError }
import cats.data.EitherT
import cats.Traverse
import cats.Eval

object Target extends LogAbstraction {
  val emptyLogger = cats.kernel.Monoid[StructuredLogger].empty
  type F[A] = Target[A]
  val A = new Applicative[Target] {
    def pure[A](x: A): Target[A] = new TargetValue(x, emptyLogger)
    def ap[A, B](ff: Target[A => B])(fa: Target[A]): Target[B] =
      (ff, fa) match {
        case (TargetValue(f, lf), TargetValue(a, la))   => new TargetValue(f(a), lf.concat(la))
        case (TargetError(err, lf), TargetError(_, la)) => new TargetError(err, lf.concat(la))
        case (TargetValue(_, lf), TargetError(err, la)) => new TargetError(err, lf.concat(la))
      }
  }
  def pushLogger(value: StructuredLogger): Target[Unit] = new TargetValue((), value)
  def pure[T](x: T): Target[T]                          = A.pure(x)
  @deprecated("Use raiseError instead", "v0.41.2")
  def error[T](x: String): Target[T]          = raiseError(x)
  def raiseError[T](x: String): Target[T]     = new TargetError(UserError(x), emptyLogger)
  def raiseException[T](x: String): Target[T] = new TargetError(RuntimeFailure(x), emptyLogger)
  def fromOption[T](x: Option[T], default: => String): Target[T] =
    x.fold[Target[T]](new TargetError(UserError(default), emptyLogger))(new TargetValue(_, emptyLogger))
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeExtract[T](x: Target[T]): T =
    x.valueOr({ err =>
      throw new Exception(err.toString)
    })

  implicit val targetInstances = new MonadError[Target, Error] with Traverse[Target] {
    def pure[A](x: A): Target[A] = Target.A.pure(x)

    def handleErrorWith[A](fa: Target[A])(f: Error => Target[A]): Target[A] = fa match {
      case fa @ TargetValue(_, _) => fa
      case TargetError(err, la)   => f(err).prependLog(la)
    }
    def raiseError[A](e: Error): Target[A] = new TargetError(e, emptyLogger)

    def flatMap[A, B](fa: Target[A])(f: A => Target[B]): Target[B] = fa match {
      case TargetValue(a, la)   => f(a).prependLog(la)
      case TargetError(err, la) => new TargetError(err, la)
    }
    def tailRecM[A, B](a: A)(f: A => Target[Either[A, B]]): Target[B] =
      f(a) match {
        case TargetError(err, la) =>
          new TargetError(err, la)
        case TargetValue(e, la) =>
          e match {
            case Left(b)  => tailRecM(b)(f).prependLog(la)
            case Right(a) => new TargetValue(a, la)
          }
      }

    def foldLeft[A, B](fa: Target[A], b: B)(f: (B, A) => B): B = fa match {
      case TargetValue(a, _) => f(b, a)
      case TargetError(_, _) => b
    }
    def foldRight[A, B](fa: Target[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case TargetValue(a, _) => f(a, lb)
      case TargetError(_, _) => lb
    }
    def traverse[G[_], A, B](fa: Target[A])(f: A => G[B])(implicit G: Applicative[G]): G[Target[B]] = fa match {
      case TargetValue(a, la) => G.ap(G.pure[B => Target[B]](new TargetValue(_, la)))(f(a))
      case TargetError(e, la) => G.pure[Target[B]](new TargetError(e, la))
    }
  }
}

sealed abstract class Target[A] {
  def logger: StructuredLogger
  def valueOr[AA >: A](fallback: Error => AA): AA
  def toEitherT: EitherT[cats.Id, Error, A]
  def map[B](f: A => B): Target[B]
  def flatMap[B](f: A => Target[B]): Target[B]
  def recover[AA >: A](f: Error => AA): Target[AA]
  def prependLog(l: StructuredLogger): Target[A]
}

object TargetValue {
  def unapply[A](x: TargetValue[A]): Option[(A, StructuredLogger)] = Some((x.value, x.logger))
}

class TargetValue[A](val value: A, val logger: StructuredLogger) extends Target[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA  = value
  def toEitherT: EitherT[cats.Id, Error, A]        = EitherT.right[Error](cats.Monad[cats.Id].pure(value))
  def map[B](f: A => B): Target[B]                 = new TargetValue(f(value), logger)
  def flatMap[B](f: A => Target[B]): Target[B]     = f(value).prependLog(logger)
  def recover[AA >: A](f: Error => AA): Target[AA] = new TargetValue(value, logger)
  def prependLog(l: StructuredLogger)              = new TargetValue(value, l.concat(logger))
}

object TargetError {
  def unapply[A](x: TargetError[A]): Option[(Error, StructuredLogger)] = Some((x.error, x.logger))
}

class TargetError[A](val error: Error, val logger: StructuredLogger) extends Target[A] {
  def valueOr[AA >: A](fallback: Error => AA): AA  = fallback(error)
  def toEitherT: EitherT[cats.Id, Error, A]        = EitherT.left[A](cats.Monad[cats.Id].pure(error))
  def map[B](f: A => B): Target[B]                 = new TargetError(error, logger)
  def flatMap[B](f: A => Target[B]): Target[B]     = new TargetError(error, logger)
  def recover[AA >: A](f: Error => AA): Target[AA] = new TargetValue(f(error), logger)
  def prependLog(l: StructuredLogger)              = new TargetError(error, l.concat(logger))
}
