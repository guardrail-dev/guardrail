package com.twilio.guardrail.protocol.terms.client

import cats.InjectK
import cats.free.Free
import com.twilio.guardrail.{ RenderedClientOperation, StrictProtocolElems }
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.terms.RouteMeta
import com.twilio.guardrail.generators.{ Responses, ScalaParameters }

import _root_.io.swagger.models.Operation

class ClientTerms[L <: LA, F[_]](implicit I: InjectK[ClientTerm[L, ?], F]) {
  def generateClientOperation(className: List[String], tracing: Boolean, parameters: ScalaParameters[L])(
      route: RouteMeta,
      methodName: String,
      responses: Responses[L]
  ): Free[F, RenderedClientOperation[L]] =
    Free.inject[ClientTerm[L, ?], F](GenerateClientOperation[L](className, route, methodName, tracing, parameters, responses))
  def getImports(tracing: Boolean): Free[F, List[L#Import]] =
    Free.inject[ClientTerm[L, ?], F](GetImports[L](tracing))
  def getExtraImports(tracing: Boolean): Free[F, List[L#Import]] =
    Free.inject[ClientTerm[L, ?], F](GetExtraImports[L](tracing))
  def clientClsArgs(tracingName: Option[String], schemes: List[String], host: Option[String], tracing: Boolean): Free[F, List[List[L#MethodParameter]]] =
    Free.inject[ClientTerm[L, ?], F](ClientClsArgs[L](tracingName, schemes, host, tracing))
  def generateResponseDefinitions(operationId: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]): Free[F, List[L#Definition]] =
    Free.inject[ClientTerm[L, ?], F](GenerateResponseDefinitions[L](operationId, responses, protocolElems))
  def buildCompanion(clientName: String,
                     tracingName: Option[String],
                     schemes: List[String],
                     host: Option[String],
                     ctorArgs: List[List[L#MethodParameter]],
                     tracing: Boolean): Free[F, L#ObjectDefinition] =
    Free.inject[ClientTerm[L, ?], F](BuildCompanion[L](clientName, tracingName, schemes, host, ctorArgs, tracing))
  def buildClient(clientName: String,
                  tracingName: Option[String],
                  schemes: List[String],
                  host: Option[String],
                  basePath: Option[String],
                  ctorArgs: List[List[L#MethodParameter]],
                  clientCalls: List[L#Definition],
                  supportDefinitions: List[L#Definition],
                  tracing: Boolean): Free[F, L#ClassDefinition] =
    Free.inject[ClientTerm[L, ?], F](
      BuildClient[L](clientName, tracingName, schemes, host, basePath, ctorArgs, clientCalls, supportDefinitions, tracing)
    )
}

object ClientTerms {
  implicit def enumTerms[L <: LA, F[_]](implicit I: InjectK[ClientTerm[L, ?], F]): ClientTerms[L, F] =
    new ClientTerms[L, F]
}
