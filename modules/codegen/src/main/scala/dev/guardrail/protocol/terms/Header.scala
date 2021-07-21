package dev.guardrail.protocol.terms

import dev.guardrail.languages.LA

class Header[L <: LA](val name: String, val isRequired: Boolean, val tpe: L#Type, val term: L#TermName) {
  override def toString: String = s"Header($name, $isRequired, $tpe, $term)"
}

object Header {
  def unapply[L <: LA](header: Header[L]): Option[(String, Boolean, L#Type, L#TermName)] = Some((header.name, header.isRequired, header.tpe, header.term))
}

class Headers[L <: LA](val value: List[Header[L]])
