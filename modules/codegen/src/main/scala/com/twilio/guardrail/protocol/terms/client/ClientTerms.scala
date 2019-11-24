package com.twilio.guardrail.protocol.terms.client

import cats.InjectK
import cats.data.NonEmptyList
import cats.free.Free
import com.twilio.guardrail.generators.ScalaParameters
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.Responses
import com.twilio.guardrail.terms.{ RouteMeta, SecurityScheme }
import com.twilio.guardrail.{ RenderedClientOperation, StaticDefns, StrictProtocolElems, SupportDefinition }
import java.net.URI

class ClientTerms[L <: LA, F[_]](implicit I: InjectK[ClientTerm[L, ?], F]) {
  def generateClientOperation(className: List[String], tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]], parameters: ScalaParameters[L])(
      route: RouteMeta,
      methodName: String,
      responses: Responses[L]
  ): Free[F, RenderedClientOperation[L]] =
    Free.inject[ClientTerm[L, ?], F](GenerateClientOperation[L](className, route, methodName, tracing, parameters, responses, securitySchemes))
  def getImports(tracing: Boolean): Free[F, List[L#Import]] =
    Free.inject[ClientTerm[L, ?], F](GetImports[L](tracing))
  def getExtraImports(tracing: Boolean): Free[F, List[L#Import]] =
    Free.inject[ClientTerm[L, ?], F](GetExtraImports[L](tracing))
  def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): Free[F, List[List[L#MethodParameter]]] =
    Free.inject[ClientTerm[L, ?], F](ClientClsArgs[L](tracingName, serverUrls, tracing))
  def generateResponseDefinitions(operationId: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]): Free[F, List[L#Definition]] =
    Free.inject[ClientTerm[L, ?], F](GenerateResponseDefinitions[L](operationId, responses, protocolElems))
  def generateSupportDefinitions(tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]]): Free[F, List[SupportDefinition[L]]] =
    Free.inject[ClientTerm[L, ?], F](GenerateSupportDefinitions[L](tracing, securitySchemes))
  def buildStaticDefns(
      clientName: String,
      tracingName: Option[String],
      serverUrls: Option[NonEmptyList[URI]],
      ctorArgs: List[List[L#MethodParameter]],
      tracing: Boolean
  ): Free[F, StaticDefns[L]] =
    Free.inject[ClientTerm[L, ?], F](BuildStaticDefns[L](clientName, tracingName, serverUrls, ctorArgs, tracing))
  def buildClient(
      clientName: String,
      tracingName: Option[String],
      serverUrls: Option[NonEmptyList[URI]],
      basePath: Option[String],
      ctorArgs: List[List[L#MethodParameter]],
      clientCalls: List[L#Definition],
      supportDefinitions: List[L#Definition],
      tracing: Boolean
  ): Free[F, NonEmptyList[Either[L#Trait, L#ClassDefinition]]] =
    Free.inject[ClientTerm[L, ?], F](
      BuildClient[L](clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing)
    )
}

object ClientTerms {
  implicit def enumTerms[L <: LA, F[_]](implicit I: InjectK[ClientTerm[L, ?], F]): ClientTerms[L, F] =
    new ClientTerms[L, F]
}
