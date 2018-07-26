package com.twilio.guardrail.swagger

import scala.meta.{ Init, Term, Tree, Type }
import scala.util.matching.Regex

object Escape {
  val Unbacktick: Regex     = "^`(.*)`$".r
  val LeadingNumeric: Regex = "^[0-9\"]".r
  val InvalidSymbols: Regex = "[-`\"'()\\.]".r
  val ReservedWords: Set[String] = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "macro",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
    "_",
    ":",
    "=",
    "=>",
    "<-",
    "<:",
    "<%",
    ">:",
    "#",
    "@"
  )

  def escapeTree[T <: Tree]: T => T =
    _.transform({
      case Term.Name(name) => Term.Name(escapeReserved(name))
      case p @ Term.Param(_, Term.Name(name), _, _) =>
        p.copy(name = Term.Name(escapeReserved(name)))
      case Type.Name(name) => Type.Name(escapeReserved(name))
      case ctor @ Init(Type.Name(name), _, _) if name != "this" =>
        ctor.copy(tpe = Type.Name(escapeReserved(name))) // Literal "this" in ctor names is OK
    }).asInstanceOf[T]

  def escapeReserved: String => String = {
    case name if Unbacktick.findFirstMatchIn(name).nonEmpty => name
    case name if name.contains(' ') =>
      name // scala.meta will automatically escape. See `EscapeTreeSpec.scala`
    case name if ReservedWords.contains(name)                   => s"`$name`"
    case name if InvalidSymbols.findFirstMatchIn(name).nonEmpty => s"`$name`"
    case name if LeadingNumeric.findFirstMatchIn(name).nonEmpty => s"`$name`"
    case name                                                   => name
  }
}
