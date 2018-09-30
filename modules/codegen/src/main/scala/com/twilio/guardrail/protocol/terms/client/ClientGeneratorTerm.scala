package com.twilio.guardrail.protocol.terms.client

import com.twilio.guardrail.{ RenderedClientOperation, StrictProtocolElems }
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._
import _root_.io.swagger.models.Operation

sealed trait ClientTerm[T]
case class GenerateClientOperation(className: List[String], route: RouteMeta, tracing: Boolean, protocolElems: List[StrictProtocolElems])
    extends ClientTerm[RenderedClientOperation]
case class GetImports(tracing: Boolean)                                                                              extends ClientTerm[List[Import]]
case class GetExtraImports(tracing: Boolean)                                                                         extends ClientTerm[List[Import]]
case class ClientClsArgs(tracingName: Option[String], schemes: List[String], host: Option[String], tracing: Boolean) extends ClientTerm[List[List[Term.Param]]]
case class GenerateResponseDefinitions(operation: Operation, protocolElems: List[StrictProtocolElems])               extends ClientTerm[List[Defn]]
case class BuildCompanion(clientName: String,
                          tracingName: Option[String],
                          schemes: List[String],
                          host: Option[String],
                          ctorArgs: List[List[Term.Param]],
                          tracing: Boolean)
    extends ClientTerm[Defn.Object]
case class BuildClient(clientName: String,
                       tracingName: Option[String],
                       schemes: List[String],
                       host: Option[String],
                       basePath: Option[String],
                       ctorArgs: List[List[Term.Param]],
                       clientCalls: List[Defn],
                       supportDefinitions: List[Defn],
                       tracing: Boolean)
    extends ClientTerm[Defn.Class]
