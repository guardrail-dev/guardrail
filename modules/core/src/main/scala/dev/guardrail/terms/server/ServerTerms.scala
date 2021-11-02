package dev.guardrail.terms.server

import cats.Monad
import cats.data.NonEmptyList
import io.swagger.v3.oas.models.Operation

import dev.guardrail.core.{ SupportDefinition, Tracker }
import dev.guardrail.generators.{ CustomExtractionField, LanguageParameters, RenderedRoutes, TracingField }
import dev.guardrail.languages.LA
import dev.guardrail.terms.Responses
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }

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

abstract class ServerTerms[L <: LA, F[_]](implicit Cl: CollectionsLibTerms[L, F]) { self =>
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
      MonadF: Monad[F] = self.MonadF,
      buildCustomExtractionFields: (Tracker[Operation], List[String], Boolean) => F[Option[CustomExtractionField[L]]] = self.buildCustomExtractionFields _,
      buildTracingFields: (Tracker[Operation], List[String], Boolean) => F[Option[TracingField[L]]] = self.buildTracingFields _,
      generateRoutes: (
          Boolean,
          String,
          String,
          Option[String],
          List[GenerateRouteMeta[L]],
          List[StrictProtocolElems[L]],
          Map[String, SecurityScheme[L]]
      ) => F[RenderedRoutes[L]] = self.generateRoutes _,
      getExtraRouteParams: (Boolean, Boolean) => F[List[L#MethodParameter]] = self.getExtraRouteParams _,
      generateResponseDefinitions: (String, Responses[L], List[StrictProtocolElems[L]]) => F[List[L#Definition]] = self.generateResponseDefinitions _,
      generateSupportDefinitions: (Boolean, Map[String, SecurityScheme[L]]) => F[List[SupportDefinition[L]]] = self.generateSupportDefinitions _,
      renderClass: (
          String,
          String,
          List[L#Annotation],
          List[L#Statement],
          List[L#MethodParameter],
          List[L#Definition],
          List[L#Definition],
          Boolean
      ) => F[List[L#Definition]] = self.renderClass _,
      renderHandler: (String, List[L#MethodDeclaration], List[L#Statement], List[L#Definition], Boolean) => F[L#Definition] = self.renderHandler _,
      getExtraImports: (Boolean, NonEmptyList[String]) => F[List[L#Import]] = self.getExtraImports _
  ) = {
    val newMonadF                      = MonadF
    val newBuildCustomExtractionFields = buildCustomExtractionFields
    val newBuildTracingFields          = buildTracingFields
    val newGenerateRoutes              = generateRoutes
    val newGetExtraRouteParams         = getExtraRouteParams
    val newGenerateResponseDefinitions = generateResponseDefinitions
    val newGenerateSupportDefinitions  = generateSupportDefinitions
    val newRenderClass                 = renderClass
    val newRenderHandler               = renderHandler
    val newGetExtraImports             = getExtraImports

    new ServerTerms[L, F] {
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
      def generateSupportDefinitions(tracing: Boolean, securitySchemes: Map[String, SecurityScheme[L]]) =
        newGenerateSupportDefinitions(tracing, securitySchemes)
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
}
