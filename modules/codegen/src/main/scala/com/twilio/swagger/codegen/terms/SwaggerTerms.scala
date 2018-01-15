package com.twilio.swagger.codegen
package terms

import _root_.io.swagger.models.{ModelImpl, Operation, Path}
import cats.free.{Free, Inject}
import scala.meta._

class SwaggerTerms[F[_]](implicit I: Inject[SwaggerTerm, F]) {
}
object SwaggerTerms {
  implicit def swaggerTerm[F[_]](implicit I: Inject[SwaggerTerm, F]): SwaggerTerms[F] = new SwaggerTerms[F]
}
