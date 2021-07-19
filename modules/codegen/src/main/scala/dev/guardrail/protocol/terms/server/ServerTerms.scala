package dev.guardrail.protocol.terms.server

import cats.Monad
import cats.data.NonEmptyList
import dev.guardrail.core.Tracker
import dev.guardrail.generators.LanguageParameters
import dev.guardrail.languages.LA
import dev.guardrail.protocol.terms.Responses
import dev.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }
import dev.guardrail.{ CustomExtractionField, RenderedRoutes, StrictProtocolElems, SupportDefinition, TracingField }
import io.swagger.v3.oas.models.Operation

case class GenerateRouteMeta[L <: LA](
    operationId: String,
    methodName: String,
    responseClsName: String,
    customExtractionField: Option[CustomExtractionField[L]],
    tracingField: Option[TracingField[L]],
    routeMeta: RouteMeta,
    parameters: LanguageParameters[L],
    responses: Responses[L]
)

abstract class ServerTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) {
  def MonadF: Monad[F]
  def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean): F[Option[CustomExtractionField[L]]]
  def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean): F[Option[TracingField[L]]]
  def generateRoutes(
      tracing: Boolean,
      resourceName: String,
      handlerName: String,
      basePath: Option[String],
      routes: List[GenerateRouteMeta[L]],
      protocolElems: List[StrictProtocolElems[L]],
      securitySchemes: Map[String, SecurityScheme[L]]
  ): F[RenderedRoutes[L]]
  def getExtraRouteParams(customExtraction: Boolean, tracing: Boolean): F[List[L#MethodParameter]]
  def generateResponseDefinitions(responseClsName: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]): F[List[L#Definition]]
  def generateSupportDefinitions(tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]]): F[List[SupportDefinition[L]]]
  def renderClass(
      resourceName: String,
      handlerName: String,
      annotations: List[L#Annotation],
      combinedRouteTerms: List[L#Statement],
      extraRouteParams: List[L#MethodParameter],
      responseDefinitions: List[L#Definition],
      supportDefinitions: List[L#Definition],
      customExtraction: Boolean
  ): F[List[L#Definition]]
  def renderHandler(
      handlerName: String,
      methodSigs: List[L#MethodDeclaration],
      handlerDefinitions: List[L#Statement],
      responseDefinitions: List[L#Definition],
      customExtraction: Boolean
  ): F[L#Definition]
  def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]): F[List[L#Import]]

  def copy(
      newMonadF: Monad[F] = MonadF,
      newBuildCustomExtractionFields: (Tracker[Operation], List[String], Boolean) => F[Option[CustomExtractionField[L]]] = buildCustomExtractionFields _,
      newBuildTracingFields: (Tracker[Operation], List[String], Boolean) => F[Option[TracingField[L]]] = buildTracingFields _,
      newGenerateRoutes: (
          Boolean,
          String,
          String,
          Option[String],
          List[GenerateRouteMeta[L]],
          List[StrictProtocolElems[L]],
          Map[String, SecurityScheme[L]]
      ) => F[RenderedRoutes[L]] = generateRoutes _,
      newGetExtraRouteParams: (Boolean, Boolean) => F[List[L#MethodParameter]] = getExtraRouteParams _,
      newGenerateResponseDefinitions: (String, Responses[L], List[StrictProtocolElems[L]]) => F[List[L#Definition]] = generateResponseDefinitions _,
      newGenerateSupportDefinitions: (Boolean, Map[String, SecurityScheme[L]]) => F[List[SupportDefinition[L]]] = generateSupportDefinitions _,
      newRenderClass: (
          String,
          String,
          List[L#Annotation],
          List[L#Statement],
          List[L#MethodParameter],
          List[L#Definition],
          List[L#Definition],
          Boolean
      ) => F[List[L#Definition]] = renderClass _,
      newRenderHandler: (String, List[L#MethodDeclaration], List[L#Statement], List[L#Definition], Boolean) => F[L#Definition] = renderHandler _,
      newGetExtraImports: (Boolean, NonEmptyList[String]) => F[List[L#Import]] = getExtraImports _
  ) = new ServerTerms[L, F] {
    def MonadF = newMonadF
    def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean) =
      newBuildCustomExtractionFields(operation, resourceName, customExtraction)
    def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean) =
      newBuildTracingFields(operation, resourceName, tracing)
    def generateRoutes(
        tracing: Boolean,
        resourceName: String,
        handlerName: String,
        basePath: Option[String],
        routes: List[GenerateRouteMeta[L]],
        protocolElems: List[StrictProtocolElems[L]],
        securitySchemes: Map[String, SecurityScheme[L]]
    )                                                                    = newGenerateRoutes(tracing, resourceName, handlerName, basePath, routes, protocolElems, securitySchemes)
    def getExtraRouteParams(customExtraction: Boolean, tracing: Boolean) = newGetExtraRouteParams(customExtraction, tracing)
    def generateResponseDefinitions(responseClsName: String, responses: Responses[L], protocolElems: List[StrictProtocolElems[L]]) =
      newGenerateResponseDefinitions(responseClsName, responses, protocolElems)
    def generateSupportDefinitions(tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]]) = newGenerateSupportDefinitions(tracing, securitySchemes)
    def renderClass(
        resourceName: String,
        handlerName: String,
        annotations: List[L#Annotation],
        combinedRouteTerms: List[L#Statement],
        extraRouteParams: List[L#MethodParameter],
        responseDefinitions: List[L#Definition],
        supportDefinitions: List[L#Definition],
        customExtraction: Boolean
    ): F[List[L#Definition]] =
      newRenderClass(resourceName, handlerName, annotations, combinedRouteTerms, extraRouteParams, responseDefinitions, supportDefinitions, customExtraction)
    def renderHandler(
        handlerName: String,
        methodSigs: List[L#MethodDeclaration],
        handlerDefinitions: List[L#Statement],
        responseDefinitions: List[L#Definition],
        customExtraction: Boolean
    )                                                                           = newRenderHandler(handlerName, methodSigs, handlerDefinitions, responseDefinitions, customExtraction)
    def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]) = newGetExtraImports(tracing, supportPackage)
  }
}
