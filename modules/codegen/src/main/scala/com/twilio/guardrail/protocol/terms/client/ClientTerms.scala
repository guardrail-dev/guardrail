package com.twilio.guardrail.protocol.terms.client

import cats.{ InjectK, Monad }
import cats.data.NonEmptyList
import cats.free.Free
import com.twilio.guardrail.generators.ScalaParameters
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.Responses
import com.twilio.guardrail.terms.{ RouteMeta, SecurityScheme }
import com.twilio.guardrail.{ RenderedClientOperation, StaticDefns, StrictProtocolElems, SupportDefinition }
import java.net.URI

abstract class ClientTerms[L <: LA, F[_]] {
  def MonadF: Monad[F]
  def generateClientOperation(className: List[String], tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]], parameters: ScalaParameters[L])(
      route: RouteMeta,
      methodName: String,
      responses: Responses[L]
  ): F[RenderedClientOperation[L]]
  def getImports(tracing: Boolean): F[List[L#Import]]
  def getExtraImports(tracing: Boolean): F[List[L#Import]]
  def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): F[List[List[L#MethodParameter]]]
  def generateResponseDefinitions(operationId: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]): F[List[L#Definition]]
  def generateSupportDefinitions(tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]]): F[List[SupportDefinition[L]]]
  def buildStaticDefns(
      clientName: String,
      tracingName: Option[String],
      serverUrls: Option[NonEmptyList[URI]],
      ctorArgs: List[List[L#MethodParameter]],
      tracing: Boolean
  ): F[StaticDefns[L]]
  def buildClient(
      clientName: String,
      tracingName: Option[String],
      serverUrls: Option[NonEmptyList[URI]],
      basePath: Option[String],
      ctorArgs: List[List[L#MethodParameter]],
      clientCalls: List[L#Definition],
      supportDefinitions: List[L#Definition],
      tracing: Boolean
  ): F[NonEmptyList[Either[L#Trait, L#ClassDefinition]]]

  def copy(
      newMonadF: Monad[F] = this.MonadF,
      newGenerateClientOperation: (List[String], Boolean, Map[String, SecurityScheme[L]], ScalaParameters[L]) => (
          RouteMeta,
          String,
          Responses[L]
      ) => F[RenderedClientOperation[L]] = generateClientOperation _,
      newGetImports: Boolean => F[List[L#Import]] = getImports _,
      newGetExtraImports: Boolean => F[List[L#Import]] = getExtraImports _,
      newClientClsArgs: (Option[String], Option[NonEmptyList[URI]], Boolean) => F[List[List[L#MethodParameter]]] = clientClsArgs _,
      newGenerateResponseDefinitions: (String, Responses[L], List[StrictProtocolElems[L]]) => F[List[L#Definition]] = generateResponseDefinitions _,
      newGenerateSupportDefinitions: (Boolean, Map[String, SecurityScheme[L]]) => F[List[SupportDefinition[L]]] = generateSupportDefinitions _,
      newBuildStaticDefns: (String, Option[String], Option[NonEmptyList[URI]], List[List[L#MethodParameter]], Boolean) => F[StaticDefns[L]] =
        buildStaticDefns _,
      newBuildClient: (
          String,
          Option[String],
          Option[NonEmptyList[URI]],
          Option[String],
          List[List[L#MethodParameter]],
          List[L#Definition],
          List[L#Definition],
          Boolean
      ) => F[NonEmptyList[Either[L#Trait, L#ClassDefinition]]] = buildClient _
  ): ClientTerms[L, F] = new ClientTerms[L, F] {
    def MonadF = newMonadF
    def generateClientOperation(className: List[String], tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]], parameters: ScalaParameters[L])(
        route: RouteMeta,
        methodName: String,
        responses: Responses[L]
    ) =
      newGenerateClientOperation(className, tracing, securitySchemes, parameters)(route, methodName, responses)
    def getImports(tracing: Boolean)                                                                        = newGetImports(tracing)
    def getExtraImports(tracing: Boolean)                                                                   = newGetExtraImports(tracing)
    def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean) = newClientClsArgs(tracingName, serverUrls, tracing)
    def generateResponseDefinitions(operationId: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]) =
      newGenerateResponseDefinitions(operationId, responses, protocolElems)
    def generateSupportDefinitions(tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]]): F[List[SupportDefinition[L]]] =
      newGenerateSupportDefinitions(tracing, securitySchemes)

    def buildStaticDefns(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        ctorArgs: List[List[L#MethodParameter]],
        tracing: Boolean
    ): F[StaticDefns[L]] =
      newBuildStaticDefns(clientName, tracingName, serverUrls, ctorArgs, tracing)
    def buildClient(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        basePath: Option[String],
        ctorArgs: List[List[L#MethodParameter]],
        clientCalls: List[L#Definition],
        supportDefinitions: List[L#Definition],
        tracing: Boolean
    ) =
      newBuildClient(clientName, tracingName, serverUrls, basePath, ctorArgs, clientCalls, supportDefinitions, tracing)
  }
}

object ClientTerms {
  implicit def enumTerms[L <: LA, F[_]](implicit I: InjectK[ClientTerm[L, ?], F]): ClientTerms[L, Free[F, ?]] = new ClientTerms[L, Free[F, ?]] {
    def MonadF = Free.catsFreeMonadForFree
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
}
