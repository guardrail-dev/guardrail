package dev.guardrail.core

import cats.data.{ Chain, NonEmptyChain }
import cats.syntax.all._
import cats.{ Monoid, Order, Show }

sealed abstract class LogLevel(val level: String)
object LogLevel {
  implicit object ShowLogLevel extends Show[LogLevel] {
    def show(x: LogLevel): String = x match {
      case LogLevels.Debug   => "  DEBUG"
      case LogLevels.Info    => "   INFO"
      case LogLevels.Warning => "WARNING"
      case LogLevels.Error   => "  ERROR"
      case LogLevels.Silent  => " SILENT"
    }
  }

  implicit object OrderLogLevel extends Order[LogLevel] {
    def compare(l: LogLevel, r: LogLevel): Int =
      LogLevels.members.indexOf(l) - LogLevels.members.indexOf(r)
  }
}

object LogLevels {
  case object Debug   extends LogLevel("debug")
  case object Info    extends LogLevel("info")
  case object Warning extends LogLevel("warning")
  case object Error   extends LogLevel("error")
  case object Silent  extends LogLevel("silent")

  val members: Vector[LogLevel] = Vector(Debug, Info, Warning, Error, Silent)

  def apply(value: String): Option[LogLevel] = members.find(_.level == value)
}

sealed trait StructuredLogEntry
sealed case class StructuredLogBlock(lines: NonEmptyChain[(LogLevel, String)]) extends StructuredLogEntry
sealed case class StructuredLoggerPush(next: String)                           extends StructuredLogEntry
case object StructuredLoggerPop                                                extends StructuredLogEntry
case object StructuredLoggerReset                                              extends StructuredLogEntry

case class StructuredLogger(entries: Chain[StructuredLogEntry])

object StructuredLogger extends StructuredLoggerInstances {
  def push(next: String): StructuredLogger = StructuredLogger(StructuredLoggerPush(next).pure[Chain])
  def pop: StructuredLogger                = StructuredLogger(StructuredLoggerPop.pure[Chain])
  def reset: StructuredLogger              = StructuredLogger(StructuredLoggerReset.pure[Chain])
  object Empty extends StructuredLogger(Chain.empty)
}
sealed trait StructuredLoggerInstances extends StructuredLoggerLowPriority {
  object StructuredLogEntryCombine {
    def unapply(chains: (Chain[StructuredLogEntry], Chain[StructuredLogEntry])): Option[Chain[StructuredLogEntry]] = {
      val (xs, ys) = chains
      for {
        (init, last) <- xs.initLast
        (head, tail) <- ys.uncons
        combined <- ((last, head) match {
          case (StructuredLogBlock(lastLines), StructuredLogBlock(headLines)) =>
            Some(StructuredLogBlock(lastLines ++ headLines))
          case (_, _) => None
        }).map(mid => (init :+ mid) ++ tail)
      } yield combined
    }
  }
  implicit object StructuredLoggerMonoid extends Monoid[StructuredLogger] {
    def empty: StructuredLogger = StructuredLogger(Chain.empty)
    def combine(x: StructuredLogger, y: StructuredLogger): StructuredLogger = StructuredLogger((x.entries, y.entries) match {
      case StructuredLogEntryCombine(combined) => combined
      case (xs, ys)                            => Monoid[Chain[StructuredLogEntry]].combine(xs, ys)
    })
  }
  class ShowStructuredLogger(desiredLevel: LogLevel) extends Show[StructuredLogger] {
    def show(value: StructuredLogger): String =
      value.entries
        .foldLeft((Chain.empty[(LogLevel, NonEmptyChain[String], String)], Chain.empty[String])) {
          case ((acc, newHistory), StructuredLoggerPop) =>
            (acc, newHistory.initLast.fold[Chain[String]](Chain.empty)(_._1))
          case ((acc, newHistory), StructuredLoggerPush(name)) =>
            (acc, newHistory :+ name)
          case ((acc, newHistory), StructuredLoggerReset) =>
            (acc, Chain.empty)
          case ((acc, newHistory), StructuredLogBlock(lines)) =>
            val history = NonEmptyChain.fromChain[String](newHistory).getOrElse(NonEmptyChain("<root>"))
            val nextLines: Chain[(LogLevel, NonEmptyChain[String], String)] = lines
              .filter(_._1 >= desiredLevel)
              .map { case (level, message) => (level, history, message) }
            (acc ++ nextLines, newHistory)
        }
        ._1
        .foldLeft((Chain.empty[String], Chain.empty[String])) { case ((lastHistory, messages), (level, history, message)) =>
          val showFullHistory = true
          def makePrefix(history: Vector[String]): String =
            history.foldLeft("  ") { case (a, b) =>
              (if (showFullHistory) {
                 a
               } else {
                 " " * a.length
               }) + " " + b
            }

          val historyVec         = history.toChain.toVector
          val commonPrefixLength = historyVec.length - lastHistory.toVector.zip(historyVec).takeWhile(((_: String) == (_: String)).tupled).length
          val histories = if (!showFullHistory) {
            (1 until commonPrefixLength).map(i => s"${level.show} ${makePrefix(historyVec.take(i))}")
          } else Nil
          val prefix    = s"${level.show} ${makePrefix(historyVec)}: "
          val formatted = (message.linesIterator.take(1).map(prefix + _) ++ message.linesIterator.drop(1).map((" " * prefix.length) + _)).mkString("\n")
          (history.toChain, (messages ++ Chain.fromSeq(histories)) ++ Chain(formatted))
        }
        ._2
        .toVector
        .mkString("\n")
  }

  def debug(message: String): StructuredLogger =
    StructuredLogger(Chain(StructuredLogBlock(NonEmptyChain.one((LogLevels.Debug, message)))))
  def info(message: String): StructuredLogger =
    StructuredLogger(Chain(StructuredLogBlock(NonEmptyChain.one((LogLevels.Info, message)))))
  def warning(message: String): StructuredLogger =
    StructuredLogger(Chain(StructuredLogBlock(NonEmptyChain.one((LogLevels.Warning, message)))))
  def error(message: String): StructuredLogger =
    StructuredLogger(Chain(StructuredLogBlock(NonEmptyChain.one((LogLevels.Error, message)))))
}

trait StructuredLoggerLowPriority { self: StructuredLoggerInstances =>
  implicit def createShowStructuredLogger(implicit desiredLevel: LogLevel): Show[StructuredLogger] =
    new ShowStructuredLogger(desiredLevel)
}
