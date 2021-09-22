package dev.guardrail

import dev.guardrail.generators.{ Client, Server }
import dev.guardrail.languages.LA
import dev.guardrail.core.SupportDefinition

case class CodegenDefinitions[L <: LA](
    clients: List[Client[L]],
    servers: List[Server[L]],
    supportDefinitions: List[SupportDefinition[L]],
    frameworksImplicits: Option[(L#TermName, L#ObjectDefinition)]
)
