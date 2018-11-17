package com.twilio.guardrail.protocol.terms.server

import _root_.io.swagger.models.{ Operation, Path }
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.{ RenderedRoutes, ServerRoute, StrictProtocolElems, TracingField }
import com.twilio.guardrail.languages.LA

sealed trait ServerTerm[L <: LA, T]
case class ExtractOperations[L <: LA](paths: List[(String, Path)]) extends ServerTerm[L, List[ServerRoute]]

case class GetClassName[L <: LA](operation: Operation)                                                     extends ServerTerm[L, List[String]]
case class BuildTracingFields[L <: LA](operation: Operation, resourceName: List[String], tracing: Boolean) extends ServerTerm[L, Option[TracingField[L]]]
case class GenerateRoutes[L <: LA](resourceName: String,
                                   basePath: Option[String],
                                   routes: List[(Option[TracingField[L]], ServerRoute)],
                                   protocolElems: List[StrictProtocolElems[L]])
    extends ServerTerm[L, RenderedRoutes[L]]
case class GetExtraRouteParams[L <: LA](tracing: Boolean)                                                          extends ServerTerm[L, List[L#MethodParameter]]
case class GenerateResponseDefinitions[L <: LA](operation: Operation, protocolElems: List[StrictProtocolElems[L]]) extends ServerTerm[L, List[L#Definition]]
case class RenderClass[L <: LA](className: String,
                                handlerName: String,
                                combinedRouteTerms: L#Term,
                                extraRouteParams: List[L#MethodParameter],
                                responseDefinitions: List[L#Definition],
                                supportDefinitions: List[L#Definition])
    extends ServerTerm[L, List[L#Statement]]
case class RenderHandler[L <: LA](handlerName: String, methodSigs: List[L#MethodDeclaration], handlerDefinitions: List[L#Statement])
    extends ServerTerm[L, L#Statement]
case class GetExtraImports[L <: LA](tracing: Boolean) extends ServerTerm[L, List[L#Import]]
