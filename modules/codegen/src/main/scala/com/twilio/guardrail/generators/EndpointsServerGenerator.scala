package com.twilio.guardrail
package generators

import cats.arrow.FunctionK
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.server._

object EndpointsServerGenerator {
  object ServerTermInterp extends FunctionK[ServerTerm[ScalaLanguage, ?], Target] {
    def apply[T](term: ServerTerm[ScalaLanguage, T]): Target[T] = term match {
      case GenerateResponseDefinitions(operationId, responses, protocolElems)                                                                 => ???
      case BuildTracingFields(operation, resourceName, tracing)                                                                               => ???
      case GenerateRoutes(resourceName, basePath, routes, protocolElems)                                                                      => ???
      case RenderHandler(handlerName, methodSigs, handlerDefinitions)                                                                         => ???
      case GetExtraRouteParams(tracing)                                                                                                       => ???
      case GenerateSupportDefinitions(tracing)                                                                                                => ???
      case RenderClass(resourceName, handlerName, annotations, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions) => ???
      case GetExtraImports(tracing)                                                                                                           => ???
    }
  }
}
