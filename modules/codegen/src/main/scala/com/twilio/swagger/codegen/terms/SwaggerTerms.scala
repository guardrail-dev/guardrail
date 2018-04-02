package com.twilio.swagger.codegen
package terms

import _root_.io.swagger.models.{ModelImpl, Operation, Path}
import cats.InjectK
import cats.free.Free

import scala.meta._

class SwaggerTerms[F[_]](implicit I: InjectK[SwaggerTerm, F]) {
  def extractOperations(paths: List[(String, Path)]): Free[F, List[RouteMeta]] =
    Free.inject[SwaggerTerm, F](ExtractOperations(paths))
  def getClassName(operation: Operation): Free[F, List[String]] =
    Free.inject[SwaggerTerm, F](GetClassName(operation))
}
object SwaggerTerms {
  implicit def swaggerTerm[F[_]](implicit I: InjectK[SwaggerTerm, F]): SwaggerTerms[F] = new SwaggerTerms[F]
}
