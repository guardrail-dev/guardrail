package dev.guardrail.generators

import cats.data.NonEmptyList

import dev.guardrail.core.SupportDefinition
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.StaticDefns

case class Clients[L <: LA](clients: List[Client[L]], supportDefinitions: List[SupportDefinition[L]])
case class Client[L <: LA](
    pkg: List[String],
    clientName: String,
    imports: List[L#Import],
    staticDefns: StaticDefns[L],
    client: NonEmptyList[Either[L#Trait, L#ClassDefinition]],
    responseDefinitions: List[L#Definition]
)
case class RenderedClientOperation[L <: LA](
    clientOperation: L#Definition,
    supportDefinitions: List[L#Definition]
)
