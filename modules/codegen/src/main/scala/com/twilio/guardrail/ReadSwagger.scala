package com.twilio.guardrail

import _root_.io.swagger.models.Swagger
import _root_.io.swagger.parser._
import cats._
import java.nio.file.Path
import scala.io.AnsiColor
import scala.util.Try
import cats.syntax.either._

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
    Either.fromOption(
      (for {
        absolutePath <- Try(rs.path.toAbsolutePath.toString).toOption
        swagger      <- Option(new SwaggerParser().read(absolutePath))
      } yield rs.next(swagger)),
      s"Spec file ${rs.path} is either incorrectly formatted or missing."
    )
}
