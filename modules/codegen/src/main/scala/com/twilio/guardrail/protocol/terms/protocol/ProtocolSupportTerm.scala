package com.twilio.guardrail.protocol.terms.protocol

import com.twilio.guardrail.languages.LA

case class PropMeta[L <: LA](clsName: String, tpe: L#Type)
