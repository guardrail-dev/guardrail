package dev.guardrail.terms

import dev.guardrail.languages.LA

sealed trait RenderedEnum[L <: LA]
case class RenderedStringEnum[L <: LA](values: List[(String, L#TermName, L#TermSelect)]) extends RenderedEnum[L]
case class RenderedIntEnum[L <: LA](values: List[(Int, L#TermName, L#TermSelect)])       extends RenderedEnum[L]
case class RenderedLongEnum[L <: LA](values: List[(Long, L#TermName, L#TermSelect)])     extends RenderedEnum[L]
