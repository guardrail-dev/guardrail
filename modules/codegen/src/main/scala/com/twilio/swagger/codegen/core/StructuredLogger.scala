package com.twilio.swagger.core

import cats.{Monoid, Order, Show}
import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.instances.list._

sealed abstract class LogLevel(val level: String)
object LogLevel {
  implicit object ShowLogLevel extends Show[LogLevel] {
    def show(x: LogLevel): String = x match {
      case LogLevels.Debug   => "  DEBUG"
      case LogLevels.Info    => "   INFO"
      case LogLevels.Warning => "WARNING"
      case LogLevels.Error   => "  ERROR"
    }
  }

  implicit object OrderLogLevel extends Order[LogLevel] {
    def compare(l: LogLevel, r: LogLevel): Int = LogLevels.members.indexOf(l) - LogLevels.members.indexOf(r)
  }
}

object LogLevels {
  case object Debug extends LogLevel("debug")
  case object Info extends LogLevel("info")
  case object Warning extends LogLevel("warning")
  case object Error extends LogLevel("error")

  val members = Vector(Debug, Info, Warning, Error)
}

sealed case class StructuredLogLevel(name: NonEmptyList[String], lines: NonEmptyList[(LogLevel, String)])

class StructuredLogger(val levels: List[StructuredLogLevel])
object StructuredLogger {
  implicit object StructuredLoggerMonoid extends Monoid[StructuredLogger] {
    def empty: StructuredLogger = new StructuredLogger(List.empty)
    def combine(x: StructuredLogger, y: StructuredLogger): StructuredLogger = new StructuredLogger({
      (y.levels.foldLeft(x.levels.reverse) {
        case (end :: acc, next) if end.name == next.name => StructuredLogLevel(end.name, end.lines.concat(next.lines)) :: acc
        case (acc, next) => next :: acc
      }).reverse
    })
  }

  implicit class ShowStructuredLogger(desiredLevel: LogLevel) extends Show[StructuredLogger] {
    def show(value: StructuredLogger): String = {
      value.levels.foldLeft(List.empty[String])({ case (acc, StructuredLogLevel(name, lines)) =>
        val prefix: String = name.foldLeft("  ") { case (a, b) => (" " * a.length) + " " + b }
        val section: List[String] = lines.filter(_._1 >= desiredLevel).map({ case (level, message) =>
          s"${level.show} ${prefix}: ${message}"
        })
        acc ++ section
      }).mkString("\n")
    }
  }

  def debug(name: NonEmptyList[String], message: String): StructuredLogger = new StructuredLogger(StructuredLogLevel(name, (LogLevels.Debug, message).pure[NonEmptyList]).pure[List])
  def info(name: NonEmptyList[String], message: String): StructuredLogger = new StructuredLogger(StructuredLogLevel(name, (LogLevels.Info, message).pure[NonEmptyList]).pure[List])
  def warning(name: NonEmptyList[String], message: String): StructuredLogger = new StructuredLogger(StructuredLogLevel(name, (LogLevels.Warning, message).pure[NonEmptyList]).pure[List])
  def error(name: NonEmptyList[String], message: String): StructuredLogger = new StructuredLogger(StructuredLogLevel(name, (LogLevels.Error, message).pure[NonEmptyList]).pure[List])
}
