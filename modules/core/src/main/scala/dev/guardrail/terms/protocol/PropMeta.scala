package dev.guardrail.terms.protocol

import dev.guardrail.languages.LA

case class PropMeta[L <: LA](clsName: String, tpe: L#Type)
