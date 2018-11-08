package com.twilio.guardrail
package terms

import com.twilio.guardrail.languages.LA

sealed trait ScalaTerm[L <: LA, T]
case class RenderImplicits[L <: LA](pkgName: List[String], frameworkImports: List[L#Import], jsonImports: List[L#Import], customImports: List[L#Import])
    extends ScalaTerm[L, L#FileContents]
case class RenderFrameworkImplicits[L <: LA](pkgName: List[String],
                                             frameworkImports: List[L#Import],
                                             jsonImports: List[L#Import],
                                             frameworkImplicits: L#ObjectDefinition)
    extends ScalaTerm[L, L#FileContents]
