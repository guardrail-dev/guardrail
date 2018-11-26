package com.twilio.guardrail.protocol.terms.client

import com.twilio.guardrail.{ RenderedClientOperation, StrictProtocolElems }
import com.twilio.guardrail.generators.Responses
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.generators.GeneratorSettings

import _root_.io.swagger.models.Operation

sealed trait ClientTerm[L <: LA, T]
case class GenerateClientOperation[L <: LA](className: List[String],
                                            route: RouteMeta,
                                            methodName: String,
                                            tracing: Boolean,
                                            protocolElems: List[StrictProtocolElems[L]],
                                            responses: Responses[L])
    extends ClientTerm[L, RenderedClientOperation[L]]
case class GetImports[L <: LA](tracing: Boolean)      extends ClientTerm[L, List[L#Import]]
case class GetExtraImports[L <: LA](tracing: Boolean) extends ClientTerm[L, List[L#Import]]
case class ClientClsArgs[L <: LA](tracingName: Option[String], schemes: List[String], host: Option[String], tracing: Boolean)
    extends ClientTerm[L, List[List[L#MethodParameter]]]
case class GenerateResponseDefinitions[L <: LA](operationId: String, operation: Operation, protocolElems: List[StrictProtocolElems[L]])
    extends ClientTerm[L, List[L#Definition]]
case class BuildCompanion[L <: LA](clientName: String,
                                   tracingName: Option[String],
                                   schemes: List[String],
                                   host: Option[String],
                                   ctorArgs: List[List[L#MethodParameter]],
                                   tracing: Boolean)
    extends ClientTerm[L, L#ObjectDefinition]
case class BuildClient[L <: LA](clientName: String,
                                tracingName: Option[String],
                                schemes: List[String],
                                host: Option[String],
                                basePath: Option[String],
                                ctorArgs: List[List[L#MethodParameter]],
                                clientCalls: List[L#Definition],
                                supportDefinitions: List[L#Definition],
                                tracing: Boolean)
    extends ClientTerm[L, L#ClassDefinition]
