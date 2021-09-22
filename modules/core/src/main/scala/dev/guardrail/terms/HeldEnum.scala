package dev.guardrail.terms

sealed trait HeldEnum
case class IntHeldEnum(value: List[Int])       extends HeldEnum
case class LongHeldEnum(value: List[Long])     extends HeldEnum
case class StringHeldEnum(value: List[String]) extends HeldEnum
