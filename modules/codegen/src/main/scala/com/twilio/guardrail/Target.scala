package com.twilio.guardrail

import com.twilio.swagger.core.StructuredLogger
import cats.{ Applicative, MonadError }
import cats.Traverse
import cats.Eval
import cats.implicits._

object Target {
  def pushLogger(value: StructuredLogger): Target[Unit] = new TargetValue((), value)
  def pure[T](x: T): Target[T]                          = new TargetValue(x, StructuredLogger.Empty)

  def raiseError[T](x: Error): Target[T]      = new TargetError(x, StructuredLogger.Empty)
  def raiseUserError[T](x: String): Target[T] = raiseError(UserError(x))
  def raiseException[T](x: String): Target[T] = raiseError(RuntimeFailure(x))
  def fromOption[T](x: Option[T], default: => Error): Target[T] =
    x.fold[Target[T]](new TargetError(default, StructuredLogger.Empty))(new TargetValue(_, StructuredLogger.Empty))

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeExtract[T](x: Target[T]): T =
    x.valueOr({ err =>
      throw new Exception(err.toString)
    })

  implicit val targetInstances: MonadError[Target, Error] with Traverse[Target] = new MonadError[Target, Error] with Traverse[Target] {
    def pure[A](x: A): Target[A] = new TargetValue(x, StructuredLogger.Empty)

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

  object log {
    def push(name: String): Target[Unit] = pushLogger(StructuredLogger.push(name))
    def pop: Target[Unit]                = pushLogger(StructuredLogger.pop)
    def function[A](name: String): Target[A] => Target[A] = { func =>
      (push(name) *> func) <* pop
    }
    def debug(message: String): Target[Unit]   = pushLogger(StructuredLogger.debug(message))
    def info(message: String): Target[Unit]    = pushLogger(StructuredLogger.info(message))
    def warning(message: String): Target[Unit] = pushLogger(StructuredLogger.warning(message))
    def error(message: String): Target[Unit]   = pushLogger(StructuredLogger.error(message))
  }
}

sealed abstract class Target[A](val logEntries: StructuredLogger) {
  def valueOr[AA >: A](fallback: Error => AA): AA
  def recover[AA >: A](f: Error => AA): Target[AA]
  def fold[B](fail: Error => B, pass: A => B): B
  def prependLogger(lastLogs: StructuredLogger): Target[A]
  def leftFlatMap[AA >: A](f: Error => Target[AA]): Target[AA]
}
object TargetValue {
  def unapply[A](x: TargetValue[A]): Option[(A, StructuredLogger)] = Some((x.value, x.logEntries))
}
class TargetValue[A](val value: A, logEntries: StructuredLogger) extends Target[A](logEntries) {
  def valueOr[AA >: A](fallback: Error => AA): AA              = value
  def recover[AA >: A](f: Error => AA): Target[AA]             = new TargetValue(value, logEntries)
  def fold[B](fail: Error => B, pass: A => B): B               = pass(value)
  def prependLogger(lastLogs: StructuredLogger): Target[A]     = new TargetValue(value, lastLogs |+| logEntries)
  def leftFlatMap[AA >: A](f: Error => Target[AA]): Target[AA] = new TargetValue(value, logEntries)
}
object TargetError {
  def unapply[A](x: TargetError[A]): Option[(Error, StructuredLogger)] = Some((x.error, x.logEntries))
}
class TargetError[A](val error: Error, logEntries: StructuredLogger) extends Target[A](logEntries) {
  def valueOr[AA >: A](fallback: Error => AA): AA              = fallback(error)
  def recover[AA >: A](f: Error => AA): Target[AA]             = new TargetValue(f(error), logEntries)
  def fold[B](fail: Error => B, pass: A => B): B               = fail(error)
  def prependLogger(lastLogs: StructuredLogger): Target[A]     = new TargetError(error, lastLogs |+| logEntries)
  def leftFlatMap[AA >: A](f: Error => Target[AA]): Target[AA] = f(error).prependLogger(logEntries)
}
