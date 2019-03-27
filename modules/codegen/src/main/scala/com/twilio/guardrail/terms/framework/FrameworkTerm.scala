package com.twilio.guardrail
package terms.framework

import com.twilio.guardrail.languages.LA

sealed trait FrameworkTerm[L <: LA, T]
case class GetFrameworkImports[L <: LA](tracing: Boolean) extends FrameworkTerm[L, List[L#Import]]
case class GetFrameworkImplicits[L <: LA]()               extends FrameworkTerm[L, Option[(L#TermName, L#ObjectDefinition)]]
case class GetFrameworkDefinitions[L <: LA]()             extends FrameworkTerm[L, List[(L#TermName, L#ClassDefinition)]]
case class LookupStatusCode[L <: LA](key: String)         extends FrameworkTerm[L, (Int, L#TermName)]
case class FileType[L <: LA](format: Option[String])      extends FrameworkTerm[L, L#Type]
case class ObjectType[L <: LA](format: Option[String])    extends FrameworkTerm[L, L#Type]
