package com.twilio.swagger.codegen
package terms

import scala.meta._

sealed trait ScalaTerm[T]
case class RenderImplicits(pkgName: List[String], frameworkImports: List[Import], jsonImports: List[Import], customImports: List[Import]) extends ScalaTerm[Source]
