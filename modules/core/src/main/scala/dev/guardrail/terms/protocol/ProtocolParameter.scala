package dev.guardrail.terms.protocol

import dev.guardrail.core.{ EmptyToNullBehaviour, RedactionBehaviour, ReifiedRawType }
import dev.guardrail.generators.RawParameterName
import dev.guardrail.languages.LA

case class ProtocolParameter[L <: LA](
    term: L#MethodParameter,
    baseType: L#Type,
    name: RawParameterName,
    dep: Option[L#TermName],
    rawType: ReifiedRawType,
    readOnlyKey: Option[String],
    emptyToNull: EmptyToNullBehaviour,
    dataRedaction: RedactionBehaviour,
    propertyRequirement: PropertyRequirement,
    defaultValue: Option[L#Term],
    propertyValidation: Option[String] = None //temporary
)
