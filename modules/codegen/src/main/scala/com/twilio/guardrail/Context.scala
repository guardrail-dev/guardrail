package com.twilio.guardrail

case class Context(framework: Option[String], tracing: Boolean)

object Context {
  val empty = Context(None, tracing = false)
}
