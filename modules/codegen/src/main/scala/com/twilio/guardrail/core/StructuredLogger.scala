package com.twilio.swagger.core

import cats.data.NonEmptyList
import cats.implicits._
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

  val members = Vector(Debug, Info, Warning, Error, Silent)

  def apply(value: String): Option[LogLevel] = members.find(_.level == value)
}

sealed trait StructuredLogEntry
sealed case class StructuredLogBlock(lines: NonEmptyList[(LogLevel, String)]) extends StructuredLogEntry
sealed case class StructuredLoggerPush(next: String)                          extends StructuredLogEntry
case object StructuredLoggerPop                                               extends StructuredLogEntry
case object StructuredLoggerReset                                             extends StructuredLogEntry

case class StructuredLogger(entries: List[StructuredLogEntry])

object StructuredLogger extends StructuredLoggerInstances {
  def push(next: String): StructuredLogger = StructuredLogger(StructuredLoggerPush(next).pure[List])
  def pop: StructuredLogger                = StructuredLogger(StructuredLoggerPop.pure[List])
  def reset: StructuredLogger              = StructuredLogger(StructuredLoggerReset.pure[List])
}
sealed trait StructuredLoggerInstances extends StructuredLoggerLowPriority {
  implicit object StructuredLoggerMonoid extends Monoid[StructuredLogger] {
    def empty                                             = StructuredLogger(Nil)
    def combine(x: StructuredLogger, y: StructuredLogger) = StructuredLogger(Monoid[List[StructuredLogEntry]].combine(x.entries, y.entries))
  }
  class ShowStructuredLogger(desiredLevel: LogLevel) extends Show[StructuredLogger] {
    def show(value: StructuredLogger): String =
      value.entries
        .foldLeft((List.empty[(LogLevel, NonEmptyList[String], String)], List.empty[String]))({
          case ((acc, newHistory), StructuredLoggerPop) =>
            (acc, newHistory.take(newHistory.length - 1))
          case ((acc, newHistory), StructuredLoggerPush(name)) =>
            (acc, newHistory :+ name)
          case ((acc, newHistory), StructuredLoggerReset) =>
            (acc, Nil)
          case ((acc, newHistory), StructuredLogBlock(lines)) =>
            val newAcc = acc ++ lines
              .filter(_._1 >= desiredLevel)
              .map({
                case (level, message) =>
                  (level, NonEmptyList.fromList(newHistory).getOrElse(NonEmptyList("<root>", Nil)), message)
              })
            (newAcc, newHistory)
        })
        ._1
        .foldLeft((List.empty[String], List.empty[String]))({
          case ((lastHistory, messages), (level, history, message)) =>
            val showFullHistory = true
            def makePrefix(history: List[String]): String =
              history.foldLeft("  ") {
                case (a, b) =>
                  (if (showFullHistory) { a } else { (" " * a.length) }) + " " + b
              }

            val commonPrefixLength = history.length - lastHistory.zip(history.toList).takeWhile(((_: String) == (_: String)).tupled).length
            val histories = if (!showFullHistory) {
              (1 until commonPrefixLength).map(i => s"${level.show} ${makePrefix(history.toList.take(i))}")
            } else Nil
            val prefix    = s"${level.show} ${makePrefix(history.toList)}: "
            val formatted = (message.linesIterator.take(1).map(prefix + _) ++ message.linesIterator.drop(1).map((" " * prefix.length) + _)).mkString("\n")
            (history.toList, (messages ++ histories) ++ List(formatted))
        })
        ._2
        .mkString("\n")
  }

  def debug(message: String): StructuredLogger =
    StructuredLogger(List(StructuredLogBlock(NonEmptyList.one((LogLevels.Debug, message)))))
  def info(message: String): StructuredLogger =
    StructuredLogger(List(StructuredLogBlock(NonEmptyList.one((LogLevels.Info, message)))))
  def warning(message: String): StructuredLogger =
    StructuredLogger(List(StructuredLogBlock(NonEmptyList.one((LogLevels.Warning, message)))))
  def error(message: String): StructuredLogger =
    StructuredLogger(List(StructuredLogBlock(NonEmptyList.one((LogLevels.Error, message)))))
}

trait StructuredLoggerLowPriority { self: StructuredLoggerInstances =>
  implicit def createShowStructuredLogger(implicit desiredLevel: LogLevel): Show[StructuredLogger] =
    new ShowStructuredLogger(desiredLevel)
}
