package com.twilio.guardrail

import java.nio.file.Path
import java.util

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.core.models.ParseOptions

case class ReadSwagger[T](path: Path, next: OpenAPI => T)
object ReadSwagger {
  def readSwagger[T](rs: ReadSwagger[Target[T]]): CoreTarget[T] =
    if (rs.path.toFile.exists()) {
      val opts = new ParseOptions()
      opts.setResolve(true)
      CoreTarget
        .fromOption(
          Option(new OpenAPIParser().readLocation(rs.path.toAbsolutePath.toString, new util.LinkedList(), opts).getOpenAPI),
          UserError(s"Spec file ${rs.path} is incorrectly formatted.")
        )
        .flatMap(rs.next(_).toEitherT)
    } else {
      CoreTarget.raiseError(UserError(s"Spec file ${rs.path} does not exist."))
    }
}
