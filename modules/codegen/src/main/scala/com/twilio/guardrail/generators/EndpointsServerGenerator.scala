package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server._

object EndpointsServerGenerator {
  object ServerTermInterp extends FunctionK[ServerTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: ServerTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateResponseDefinitions(operationId, responses, protocolElems) => Target.raiseUserError("endpoints server generation is not currently supported")
      case BuildTracingFields(operation, resourceName, tracing)               => Target.raiseUserError("endpoints server generation is not currently supported")
      case GenerateRoutes(tracing, resourceName, basePath, routes, protocolElems, securitySchemes) =>
        Target.raiseUserError("endpoints server generation is not currently supported")
      case RenderHandler(handlerName, methodSigs, handlerDefinitions, responseDefinitions) =>
        Target.raiseUserError("endpoints server generation is not currently supported")
      case GetExtraRouteParams(tracing)                         => Target.raiseUserError("endpoints server generation is not currently supported")
      case GenerateSupportDefinitions(tracing, securitySchemes) => Target.raiseUserError("endpoints server generation is not currently supported")
      case RenderClass(resourceName, handlerName, annotations, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) =>
        Target.raiseUserError("endpoints server generation is not currently supported")
      case GetExtraImports(tracing) => Target.raiseUserError("endpoints server generation is not currently supported")
    }
  }
}
