package dev.guardrail.core

import dev.guardrail.languages.LA

case class SupportDefinition[L <: LA](className: L#TermName, imports: List[L#Import], definition: List[L#Definition], insideDefinitions: Boolean = true)
