package dev.guardrail.terms

import io.swagger.v3.oas.models.media._

sealed trait EnumSchema
case class NumberEnumSchema(value: Schema[Number]) extends EnumSchema
case class ObjectEnumSchema(value: Schema[Object]) extends EnumSchema
case class StringEnumSchema(value: Schema[String]) extends EnumSchema
