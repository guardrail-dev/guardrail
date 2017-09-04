package com.twilio.swagger.codegen

case class Context(
  framework: Option[String],
  tracing: Boolean
)

object Context {
  val empty = Context(None, false)
}
