package com.twilio.guardrail

import com.twilio.swagger.core.StructuredLogger
import cats.implicits._
import cats.{ Applicative, Eval, MonadError, Traverse }

object CoreTarget extends LogAbstraction {
  implicit val coreTargetInstances = new MonadError[CoreTarget, Error] with Traverse[CoreTarget] {
    def pure[A](x: A): CoreTarget[A] = new CoreTargetValue(x, StructuredLogger.Empty)

    def handleErrorWith[A](fa: CoreTarget[A])(f: com.twilio.guardrail.Error => CoreTarget[A]): CoreTarget[A] = fa match {
      case ct @ CoreTargetValue(_, _) => ct
      case CoreTargetError(err, el)   => f(err).prependLogger(el)
    }
    def raiseError[A](e: com.twilio.guardrail.Error): CoreTarget[A] = new CoreTargetError(e, StructuredLogger.Empty)

    def flatMap[A, B](fa: CoreTarget[A])(f: A => CoreTarget[B]): CoreTarget[B] = fa match {
      case CoreTargetValue(a, al)   => f(a).prependLogger(al)
      case CoreTargetError(err, el) => new CoreTargetError(err, el)
    }
    def tailRecM[A, B](a: A)(f: A => CoreTarget[Either[A, B]]): CoreTarget[B] =
      f(a) match {
        case CoreTargetError(err, el) =>
          new CoreTargetError(err, el)
        case CoreTargetValue(e, el) =>
          e match {
            case Left(b)  => tailRecM(b)(f).prependLogger(el)
            case Right(a) => new CoreTargetValue(a, el)
          }
      }

    def foldLeft[A, B](fa: CoreTarget[A], b: B)(f: (B, A) => B): B = fa match {
      case CoreTargetValue(a, _) => f(b, a)
      case CoreTargetError(_, _) => b
    }

    def foldRight[A, B](fa: CoreTarget[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case CoreTargetValue(a, _) => f(a, lb)
      case CoreTargetError(_, _) => lb
    }

    def traverse[G[_], A, B](fa: CoreTarget[A])(f: A => G[B])(implicit G: Applicative[G]): G[CoreTarget[B]] = fa match {
      case CoreTargetValue(a, al) => G.ap(G.pure[B => CoreTarget[B]](new CoreTargetValue[B](_, al)))(f(a))
      case CoreTargetError(e, el) => G.pure[CoreTarget[B]](new CoreTargetError(e, el))
    }
  }

  type F[A] = CoreTarget[A]
  val A                                                     = coreTargetInstances
  def pushLogger(value: StructuredLogger): CoreTarget[Unit] = new CoreTargetValue((), value)
  def pure[T](x: T): CoreTarget[T]                          = x.pure[CoreTarget]

  def raise[T](x: Error): CoreTarget[T] = new CoreTargetError(x, StructuredLogger.Empty)

  def fromOption[T](x: Option[T], default: => Error): CoreTarget[T] =
    x.fold[CoreTarget[T]](new CoreTargetError(default, StructuredLogger.Empty))(new CoreTargetValue[T](_, StructuredLogger.Empty))
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeExtract[T](x: CoreTarget[T]): T =
    x.valueOr({ err =>
      throw new Exception(err.toString)
    })
}

sealed abstract class CoreTarget[A](val logEntries: StructuredLogger) {
  def valueOr[AA >: A](fallback: Error => AA): AA
  def fold[B](fail: Error => B, pass: A => B): B
  def leftFlatMap[AA >: A](f: Error => CoreTarget[AA]): CoreTarget[AA]
  def prependLogger(lastLogs: StructuredLogger): CoreTarget[A]
}

object CoreTargetValue {
  def unapply[A](x: CoreTargetValue[A]): Option[(A, StructuredLogger)] = Some((x.value, x.logEntries))
}
class CoreTargetValue[A](val value: A, logEntries: StructuredLogger) extends CoreTarget[A](logEntries) {
  def valueOr[AA >: A](fallback: Error => AA): AA                      = value
  def fold[B](fail: Error => B, pass: A => B): B                       = pass(value)
  def leftFlatMap[AA >: A](f: Error => CoreTarget[AA]): CoreTarget[AA] = new CoreTargetValue(value, logEntries)
  def prependLogger(lastLogs: StructuredLogger): CoreTarget[A]         = new CoreTargetValue(value, lastLogs |+| logEntries)
}
object CoreTargetError {
  def unapply[A](x: CoreTargetError[A]): Option[(Error, StructuredLogger)] = Some((x.error, x.logEntries))
}
class CoreTargetError[A](val error: Error, logEntries: StructuredLogger) extends CoreTarget[A](logEntries) {
  def valueOr[AA >: A](fallback: Error => AA): AA                      = fallback(error)
  def fold[B](fail: Error => B, pass: A => B): B                       = fail(error)
  def leftFlatMap[AA >: A](f: Error => CoreTarget[AA]): CoreTarget[AA] = f(error).prependLogger(logEntries)
  def prependLogger(lastLogs: StructuredLogger): CoreTarget[A]         = new CoreTargetError(error, lastLogs |+| logEntries)
}
