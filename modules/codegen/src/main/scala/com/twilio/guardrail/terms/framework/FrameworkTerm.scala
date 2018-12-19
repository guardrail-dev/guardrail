package com.twilio.guardrail
package terms.framework

import com.twilio.guardrail.languages.LA

sealed trait FrameworkTerm[L <: LA, T]
case class GetFrameworkImports[L <: LA](tracing: Boolean) extends FrameworkTerm[L, List[L#Import]]
case class GetFrameworkImplicits[L <: LA]()               extends FrameworkTerm[L, (L#TermName, L#ObjectDefinition)]
case class LookupStatusCode[L <: LA](key: String)         extends FrameworkTerm[L, (Int, L#TermName)]
case class FileType[L <: LA](format: Option[String])      extends FrameworkTerm[L, L#Type]
case class ObjectType[L <: LA](format: Option[String])    extends FrameworkTerm[L, L#Type]
