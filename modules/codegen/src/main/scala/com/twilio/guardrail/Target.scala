package com.twilio.guardrail

import com.twilio.swagger.core.StructuredLogger
import cats.{ Applicative, MonadError }
import cats.Traverse
import cats.Eval
import cats.implicits._

object Target extends LogAbstraction {
  type F[A] = Target[A]
  val A = new Applicative[Target] {
    def pure[A](x: A): Target[A] = new TargetValue(x, StructuredLogger.Empty)
    def ap[A, B](ff: Target[A => B])(fa: Target[A]): Target[B] =
      (ff, fa) match {
        case (TargetValue(f, fl), TargetValue(a, al)) => new TargetValue(f(a), fl |+| al)
        case (TargetError(err, la), _)                => new TargetError(err, la)
        case (fst, TargetError(err, la))              => new TargetError(err, la).prependLogger(fst.logEntries)
      }
  }
  def pushLogger(value: StructuredLogger): Target[Unit] = new TargetValue((), value)
  def pure[T](x: T): Target[T]                          = A.pure(x)
  @deprecated("Use raiseError instead", "v0.41.2")
  def error[T](x: String): Target[T]          = raiseError(x)
  def raiseError[T](x: String): Target[T]     = new TargetError(UserError(x), StructuredLogger.Empty)
  def raiseException[T](x: String): Target[T] = new TargetError(RuntimeFailure(x), StructuredLogger.Empty)
  def fromOption[T](x: Option[T], default: => String): Target[T] =
    x.fold[Target[T]](new TargetError(UserError(default), StructuredLogger.Empty))(new TargetValue(_, StructuredLogger.Empty))
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeExtract[T](x: Target[T]): T =
    x.valueOr({ err =>
      throw new Exception(err.toString)
    })
  // .runEmptyA

  implicit val targetInstances = new MonadError[Target, Error] with Traverse[Target] {
    def pure[A](x: A): Target[A] = Target.A.pure(x)

    def handleErrorWith[A](fa: Target[A])(f: Error => Target[A]): Target[A] = fa match {
      case fa @ TargetValue(_, _) => fa
      case TargetError(err, sl)   => f(err).prependLogger(sl)
    }
    def raiseError[A](e: Error): Target[A] = new TargetError(e, StructuredLogger.Empty)

    def flatMap[A, B](fa: Target[A])(f: A => Target[B]): Target[B] = fa match {
      case TargetValue(a, la)   => f(a).prependLogger(la)
      case TargetError(err, la) => new TargetError(err, la)
    }
    def tailRecM[A, B](a: A)(f: A => Target[Either[A, B]]): Target[B] =
      f(a) match {
        case TargetError(err, la) =>
          new TargetError(err, la)
        case TargetValue(e, la) =>
          e match {
            case Left(b)  => tailRecM(b)(f).prependLogger(la)
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
      case TargetValue(a, la) => G.ap(G.pure[B => Target[B]](new TargetValue[B](_, la)))(f(a))
      case TargetError(e, la) => G.pure[Target[B]](new TargetError(e, la))
    }
  }
}

sealed abstract class Target[A](val logEntries: StructuredLogger) {
  def valueOr[AA >: A](fallback: Error => AA): AA
  def toCoreTarget: CoreTarget[A]
  def map[B](f: A => B): Target[B]
  def flatMap[B](f: A => Target[B]): Target[B]
  def recover[AA >: A](f: Error => AA): Target[AA]
  def fold[B](fail: Error => B, pass: A => B): B
  def prependLogger(lastLogs: StructuredLogger): Target[A]
}
object TargetValue {
  def unapply[A](x: TargetValue[A]): Option[(A, StructuredLogger)] = Some((x.value, x.logEntries))
}
class TargetValue[A](val value: A, logEntries: StructuredLogger) extends Target[A](logEntries) {
  def valueOr[AA >: A](fallback: Error => AA): AA          = value
  def toCoreTarget: CoreTarget[A]                          = new CoreTargetValue(value)
  def map[B](f: A => B): Target[B]                         = new TargetValue(f(value), logEntries)
  def flatMap[B](f: A => Target[B]): Target[B]             = f(value).prependLogger(logEntries)
  def recover[AA >: A](f: Error => AA): Target[AA]         = new TargetValue(value, logEntries)
  def fold[B](fail: Error => B, pass: A => B): B           = pass(value)
  def prependLogger(lastLogs: StructuredLogger): Target[A] = new TargetValue(value, lastLogs |+| logEntries)
}
object TargetError {
  def unapply[A](x: TargetError[A]): Option[(Error, StructuredLogger)] = Some((x.error, x.logEntries))
}
class TargetError[A](val error: Error, logEntries: StructuredLogger) extends Target[A](logEntries) {
  def valueOr[AA >: A](fallback: Error => AA): AA          = fallback(error)
  def toCoreTarget: CoreTarget[A]                          = new CoreTargetError(error)
  def map[B](f: A => B): Target[B]                         = new TargetError(error, logEntries)
  def flatMap[B](f: A => Target[B]): Target[B]             = new TargetError(error, logEntries)
  def recover[AA >: A](f: Error => AA): Target[AA]         = new TargetValue(f(error), StructuredLogger.Empty)
  def fold[B](fail: Error => B, pass: A => B): B           = fail(error)
  def prependLogger(lastLogs: StructuredLogger): Target[A] = new TargetError(error, lastLogs |+| logEntries)
}
