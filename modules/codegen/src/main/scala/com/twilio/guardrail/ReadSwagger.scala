package com.twilio.guardrail

import java.nio.file.Path

import _root_.io.swagger.models.Swagger
import _root_.io.swagger.parser._
import cats._

import scala.io.AnsiColor

case class ReadSwagger[T](path: Path, next: Swagger => T)
object ReadSwagger {
  @deprecated("0.37.1", "Hiding the error result prevents build tools from failing on file read")
  def unsafeReadSwagger[T: Monoid](rs: ReadSwagger[T]): T =
    readSwagger(rs)
      .fold({ err =>
        println(s"${AnsiColor.RED}${err}${AnsiColor.RESET}")
        Monoid.empty[T]
      }, identity)

  def readSwagger[T](rs: ReadSwagger[T]): Either[String, T] =
    if (rs.path.toFile.exists()) {
      Option(new SwaggerParser().read(rs.path.toAbsolutePath.toString))
        .map(rs.next)
        .toRight(s"Spec file ${rs.path} is incorrectly formatted.")
    } else {
      Left(s"Spec file ${rs.path} does not exist.")
    }
}
