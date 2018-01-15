package com.twilio.swagger.codegen
package terms.framework

import scala.meta._

sealed trait FrameworkTerm[T]
case class GetFrameworkImports(tracing: Boolean) extends FrameworkTerm[List[Import]]
