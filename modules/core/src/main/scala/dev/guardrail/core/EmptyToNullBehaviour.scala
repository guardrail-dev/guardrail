package dev.guardrail.core

sealed trait EmptyToNullBehaviour
case object EmptyIsNull  extends EmptyToNullBehaviour
case object EmptyIsEmpty extends EmptyToNullBehaviour
