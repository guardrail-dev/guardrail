package dev.guardrail.terms

import scala.language.implicitConversions

sealed trait HeldEnum

class IntHeldEnum private (val value: List[Int]) extends HeldEnum
object IntHeldEnum {
  def apply(values: List[Number]): IntHeldEnum                 = new IntHeldEnum(values.flatMap(Option(_)).map(_.intValue()))
  implicit def unapply(heldEnum: IntHeldEnum): Some[List[Int]] = Some(heldEnum.value)
}

class LongHeldEnum private (val value: List[Long]) extends HeldEnum
object LongHeldEnum {
  def apply(values: List[Number]): LongHeldEnum                  = new LongHeldEnum(values.flatMap(Option(_)).map(_.longValue()))
  implicit def unapply(heldEnum: LongHeldEnum): Some[List[Long]] = Some(heldEnum.value)
}

class StringHeldEnum private (val value: List[String]) extends HeldEnum
object StringHeldEnum {
  def fromNumbers(values: List[Number]): StringHeldEnum              = new StringHeldEnum(values.flatMap(Option(_)).map(_.toString))
  def apply(values: List[String]): StringHeldEnum                    = new StringHeldEnum(values.flatMap(Option(_)))
  implicit def unapply(heldEnum: StringHeldEnum): Some[List[String]] = Some(heldEnum.value)
}
