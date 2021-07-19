package dev.guardrail.protocol.terms.protocol

import dev.guardrail.languages.LA

case class PropMeta[L <: LA](clsName: String, tpe: L#Type)
