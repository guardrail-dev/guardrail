package com.twilio.guardrail
package terms

import _root_.io.swagger.models._
import scala.meta._

case class RouteMeta(path: String, method: HttpMethod, operation: Operation)

sealed trait SwaggerTerm[T]
case class ExtractOperations(paths: List[(String, Path)]) extends SwaggerTerm[List[RouteMeta]]
case class GetClassName(operation: Operation)             extends SwaggerTerm[List[String]]
