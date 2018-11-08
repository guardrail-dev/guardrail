package com.twilio.guardrail
package terms

import _root_.io.swagger.models._
import com.twilio.guardrail.languages.LA

case class RouteMeta(path: String, method: HttpMethod, operation: Operation)

sealed trait SwaggerTerm[L <: LA, T]
case class ExtractOperations[L <: LA](paths: List[(String, Path)]) extends SwaggerTerm[L, List[RouteMeta]]
case class GetClassName[L <: LA](operation: Operation)             extends SwaggerTerm[L, List[String]]
