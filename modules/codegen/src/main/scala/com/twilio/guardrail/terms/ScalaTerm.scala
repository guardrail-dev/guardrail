package com.twilio.guardrail
package terms

import scala.meta._

sealed trait ScalaTerm[T]
case class RenderImplicits(pkgName: List[String], frameworkImports: List[Import], jsonImports: List[Import], customImports: List[Import])
    extends ScalaTerm[Source]
case class RenderFrameworkImplicits(
    pkgName: List[String],
    frameworkImports: List[Import],
    jsonImports: List[Import],
    frameworkImplicits: Defn.Object
) extends ScalaTerm[Source]
