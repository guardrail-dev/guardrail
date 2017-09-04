package com.twilio.swagger.codegen
package terms

import scala.collection.immutable.Seq
import scala.meta._

sealed trait ScalaTerm[T]
case class RenderImplicits(pkgName: Seq[String], frameworkImports: Seq[Import], jsonImports: Seq[Import]) extends ScalaTerm[Source]
