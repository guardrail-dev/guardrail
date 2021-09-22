package dev.guardrail.core

sealed trait RedactionBehaviour
case object DataVisible  extends RedactionBehaviour
case object DataRedacted extends RedactionBehaviour
