package com.twilio.guardrail.generators.Java

import cats.~>
import com.twilio.guardrail.Target
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.client._

object SpringMvcClientGenerator {

  object ClientTermInterp extends (ClientTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ClientTerm[JavaLanguage, T]): Target[T] = term match {
      case GenerateClientOperation(className, route, methodName, tracing, parameters, responses, securitySchemes) =>
        Target.raiseUserError("spring client generation is not currently supported")

      case GetImports(tracing) =>
        Target.raiseUserError("spring client generation is not currently supported")

      case GetExtraImports(tracing) =>
        Target.raiseUserError("spring client generation is not currently supported")

      case ClientClsArgs(tracingName, serverUrls, tracing) =>
        Target.raiseUserError("spring client generation is not currently supported")

      case GenerateResponseDefinitions(operationId, responses, protocolElems) =>
        Target.raiseUserError("spring client generation is not currently supported")

      case GenerateSupportDefinitions(tracing, securitySchemes) =>
        Target.raiseUserError("spring client generation is not currently supported")

      case BuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing) =>
        Target.raiseUserError("spring client generation is not currently supported")

      case BuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing) =>
        Target.raiseUserError("spring client generation is not currently supported")
    }
  }
}
