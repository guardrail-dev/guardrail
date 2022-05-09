package dev.guardrail.terms.client

import cats.Monad
import cats.data.NonEmptyList
import java.net.URI

import dev.guardrail.core.SupportDefinition
import dev.guardrail.generators.LanguageParameters
import dev.guardrail.generators.RenderedClientOperation
import dev.guardrail.languages.LA
import dev.guardrail.terms.Responses
import dev.guardrail.terms.protocol.{ StaticDefns, StrictProtocolElems }
import dev.guardrail.terms.{ RouteMeta, SecurityScheme }

abstract class ClientTerms[L <: LA, F[_]] { self =>
  def MonadF: Monad[F]
  def generateClientOperation(
      className: List[String],
      responseClsName: String,
      tracing: Boolean,
      securitySchemes: Map[String, SecurityScheme[L]],
      parameters: LanguageParameters[L]
  )(
      route: RouteMeta,
      methodName: String,
      responses: Responses[L]
  ): F[RenderedClientOperation[L]]
  def getImports(tracing: Boolean): F[List[L#Import]]
  def getExtraImports(tracing: Boolean): F[List[L#Import]]
  def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): F[List[List[L#MethodParameter]]]
  def generateResponseDefinitions(responseClsName: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]): F[List[L#Definition]]
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
      MonadF: Monad[F] = self.MonadF,
      generateClientOperation: (List[String], String, Boolean, Map[String, SecurityScheme[L]], LanguageParameters[L]) => (
          RouteMeta,
          String,
          Responses[L]
      ) => F[RenderedClientOperation[L]] = self.generateClientOperation _,
      getImports: Boolean => F[List[L#Import]] = self.getImports _,
      getExtraImports: Boolean => F[List[L#Import]] = self.getExtraImports _,
      clientClsArgs: (Option[String], Option[NonEmptyList[URI]], Boolean) => F[List[List[L#MethodParameter]]] = self.clientClsArgs _,
      generateResponseDefinitions: (String, Responses[L], List[StrictProtocolElems[L]]) => F[List[L#Definition]] = self.generateResponseDefinitions _,
      generateSupportDefinitions: (Boolean, Map[String, SecurityScheme[L]]) => F[List[SupportDefinition[L]]] = self.generateSupportDefinitions _,
      buildStaticDefns: (String, Option[String], Option[NonEmptyList[URI]], List[List[L#MethodParameter]], Boolean) => F[StaticDefns[L]] =
        self.buildStaticDefns _,
      buildClient: (
          String,
          Option[String],
          Option[NonEmptyList[URI]],
          Option[String],
          List[List[L#MethodParameter]],
          List[L#Definition],
          List[L#Definition],
          Boolean
      ) => F[NonEmptyList[Either[L#Trait, L#ClassDefinition]]] = self.buildClient _
  ): ClientTerms[L, F] = {
    val newMonadF                      = MonadF
    val newGenerateClientOperation     = generateClientOperation
    val newGetImports                  = getImports
    val newGetExtraImports             = getExtraImports
    val newClientClsArgs               = clientClsArgs
    val newGenerateResponseDefinitions = generateResponseDefinitions
    val newGenerateSupportDefinitions  = generateSupportDefinitions
    val newBuildStaticDefns            = buildStaticDefns
    val newBuildClient                 = buildClient

    new ClientTerms[L, F] {
      def MonadF = newMonadF
      def generateClientOperation(
          className: List[String],
          responseClsName: String,
          tracing: Boolean,
          securitySchemes: Map[String, SecurityScheme[L]],
          parameters: LanguageParameters[L]
      )(
          route: RouteMeta,
          methodName: String,
          responses: Responses[L]
      ) =
        newGenerateClientOperation(className, responseClsName, tracing, securitySchemes, parameters)(route, methodName, responses)
      def getImports(tracing: Boolean)      = newGetImports(tracing)
      def getExtraImports(tracing: Boolean) = newGetExtraImports(tracing)
      def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean) =
        newClientClsArgs(tracingName, serverUrls, tracing)
      def generateResponseDefinitions(responseClsName: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]) =
        newGenerateResponseDefinitions(responseClsName, responses, protocolElems)
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
}
