package com.twilio.guardrail.protocol.terms.server

import _root_.io.swagger.models.{ Operation, Path }
import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.{ RenderedRoutes, ServerRoute, StrictProtocolElems, TracingField }

import scala.meta._

sealed trait ServerTerm[T]
case class ExtractOperations(paths: List[(String, Path)]) extends ServerTerm[List[ServerRoute]]

case class GetClassName(operation: Operation)                                                     extends ServerTerm[List[String]]
case class BuildTracingFields(operation: Operation, resourceName: List[String], tracing: Boolean) extends ServerTerm[Option[TracingField]]
case class GenerateRoutes(className: List[String],
                          resourceName: String,
                          basePath: Option[String],
                          routes: List[(Option[TracingField], ServerRoute)],
                          tracing: Boolean,
                          protocolElems: List[StrictProtocolElems])
    extends ServerTerm[RenderedRoutes]
case class GetExtraRouteParams(tracing: Boolean)                                                       extends ServerTerm[List[Term.Param]]
case class GenerateResponseDefinitions(operation: Operation, protocolElems: List[StrictProtocolElems]) extends ServerTerm[List[Defn]]
case class RenderClass(className: String,
                       handlerName: String,
                       combinedRouteTerms: Term,
                       extraRouteParams: List[Term.Param],
                       responseDefinitions: List[Defn],
                       supportDefinitions: List[Defn])
    extends ServerTerm[List[Stat]]
case class RenderHandler(handlerName: String, methodSigs: List[Decl.Def], handlerDefinitions: List[Stat]) extends ServerTerm[Stat]
case class GetExtraImports(tracing: Boolean)                                                              extends ServerTerm[List[Import]]
