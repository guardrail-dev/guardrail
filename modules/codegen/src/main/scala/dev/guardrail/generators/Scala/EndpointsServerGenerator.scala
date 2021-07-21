package dev.guardrail.generators.Scala

import _root_.io.swagger.v3.oas.models.Operation
import cats.data.NonEmptyList
import cats.Monad
import dev.guardrail.core.Tracker
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.Responses
import dev.guardrail.protocol.terms.server._
import dev.guardrail.terms.{ CollectionsLibTerms, SecurityScheme }
import dev.guardrail.{ StrictProtocolElems, Target }

object EndpointsServerGenerator {
  def ServerTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ServerTerms[ScalaLanguage, Target] =
    new ServerTermInterp
  class ServerTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ServerTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def generateResponseDefinitions(responseClsName: String, responses: Responses[ScalaLanguage], protocolElems: List[StrictProtocolElems[ScalaLanguage]]) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def generateRoutes(
        tracing: Boolean,
        resourceName: String,
        handlerName: String,
        basePath: Option[String],
        routes: List[GenerateRouteMeta[ScalaLanguage]],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]],
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def renderHandler(
        handlerName: String,
        methodSigs: List[scala.meta.Decl.Def],
        handlerDefinitions: List[scala.meta.Stat],
        responseDefinitions: List[scala.meta.Defn],
        customExtraction: Boolean
    ) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def getExtraRouteParams(customExtraction: Boolean, tracing: Boolean) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def renderClass(
        resourceName: String,
        handlerName: String,
        annotations: List[scala.meta.Mod.Annot],
        combinedRouteTerms: List[scala.meta.Stat],
        extraRouteParams: List[scala.meta.Term.Param],
        responseDefinitions: List[scala.meta.Defn],
        supportDefinitions: List[scala.meta.Defn],
        customExtraction: Boolean
    ) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]) =
      Target.raiseUserError("endpoints server generation is not currently supported")
  }
}
