package com.twilio.guardrail

case class Context(framework: Option[String], tracing: Boolean, modules: List[String])

object Context {
  val empty: Context = Context(None, tracing = false, modules = List.empty)
}
