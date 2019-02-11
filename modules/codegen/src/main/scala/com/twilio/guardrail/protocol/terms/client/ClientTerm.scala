package com.twilio.guardrail.protocol.terms.client

import cats.data.NonEmptyList
import com.twilio.guardrail.generators.{ Responses, ScalaParameters }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.{ RenderedClientOperation, StaticDefns, StrictProtocolElems }
import java.net.URI

sealed trait ClientTerm[L <: LA, T]
case class GenerateClientOperation[L <: LA](className: List[String],
                                            route: RouteMeta,
                                            methodName: String,
                                            tracing: Boolean,
                                            parameters: ScalaParameters[L],
                                            responses: Responses[L])
    extends ClientTerm[L, RenderedClientOperation[L]]
case class GetImports[L <: LA](tracing: Boolean)      extends ClientTerm[L, List[L#Import]]
case class GetExtraImports[L <: LA](tracing: Boolean) extends ClientTerm[L, List[L#Import]]
case class ClientClsArgs[L <: LA](tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean)
    extends ClientTerm[L, List[List[L#MethodParameter]]]
case class GenerateResponseDefinitions[L <: LA](operationId: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]])
    extends ClientTerm[L, List[L#Definition]]
case class BuildStaticDefns[L <: LA](clientName: String,
                                     tracingName: Option[String],
                                     serverUrls: Option[NonEmptyList[URI]],
                                     ctorArgs: List[List[L#MethodParameter]],
                                     tracing: Boolean)
    extends ClientTerm[L, StaticDefns[L]]
case class BuildClient[L <: LA](clientName: String,
                                tracingName: Option[String],
                                serverUrls: Option[NonEmptyList[URI]],
                                basePath: Option[String],
                                ctorArgs: List[List[L#MethodParameter]],
                                clientCalls: List[L#Definition],
                                supportDefinitions: List[L#Definition],
                                tracing: Boolean)
    extends ClientTerm[L, L#ClassDefinition]
