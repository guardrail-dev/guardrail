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
sealed case class StructuredLogBlock(history: List[String], lines: NonEmptyList[(LogLevel, String)]) extends StructuredLogEntry
sealed case class StructuredLoggerPush(next: String) extends StructuredLogEntry
case object StructuredLoggerPop extends StructuredLogEntry
case object StructuredLoggerReset extends StructuredLogEntry

case class StructuredLogger(entries: List[StructuredLogEntry])

object StructuredLogger extends StructuredLoggerInstances {
  def push(next: String): StructuredLogger = StructuredLogger(StructuredLoggerPush(next).pure[List])
  def pop: StructuredLogger = StructuredLogger(StructuredLoggerPop.pure[List])
  def reset: StructuredLogger = StructuredLogger(StructuredLoggerReset.pure[List])
}
sealed trait StructuredLoggerInstances extends StructuredLoggerLowPriority {
  implicit object StructuredLoggerMonoid extends Monoid[StructuredLogger] {
    def empty = StructuredLogger(Nil)
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
          case ((acc, newHistory), StructuredLogBlock(name, lines)) =>
            val newAcc = acc ++ lines
              .filter(_._1 >= desiredLevel)
              .flatMap({
                case (level, message) =>
                  NonEmptyList.fromList((name.reverse ++ newHistory.reverse).reverse).map((level, _, message))
              })
            (newAcc, newHistory)
        })
        ._1
        .foldLeft((List.empty[String], List.empty[String]))({
          case ((lastHistory, messages), (level, history, message)) =>
            val showFullHistory = false
            def makePrefix(history: List[String]): String =
              history.foldLeft("  ") { case (a, b) =>
                (if (showFullHistory) { a } else { (" " * a.length) }) + " " + b
              }

            val commonPrefixLength = history.length - lastHistory.zip(history.toList).takeWhile(((_: String) == (_: String)).tupled).length
            val histories = if (! showFullHistory) {
              (1 until commonPrefixLength).map(i => s"${level.show} ${makePrefix(history.toList.take(i))}")
            } else Nil
            val formatted = s"${level.show} ${makePrefix(history.toList)}: ${message}"
            (history.toList, (messages ++ histories) ++ List(formatted))
        })
        ._2
        .mkString("\n")
  }

  def debug(name: List[String], message: String): StructuredLogger =
    StructuredLogger(StructuredLogBlock(name, (LogLevels.Debug, message).pure[NonEmptyList]).pure[List])
  def info(name: List[String], message: String): StructuredLogger =
    StructuredLogger(StructuredLogBlock(name, (LogLevels.Info, message).pure[NonEmptyList]).pure[List])
  def warning(name: List[String], message: String): StructuredLogger =
    StructuredLogger(StructuredLogBlock(name, (LogLevels.Warning, message).pure[NonEmptyList]).pure[List])
  def error(name: List[String], message: String): StructuredLogger =
    StructuredLogger(StructuredLogBlock(name, (LogLevels.Error, message).pure[NonEmptyList]).pure[List])
}

trait StructuredLoggerLowPriority { self: StructuredLoggerInstances =>
  implicit def createShowStructuredLogger(implicit desiredLevel: LogLevel): Show[StructuredLogger] =
    new ShowStructuredLogger(desiredLevel)
}
