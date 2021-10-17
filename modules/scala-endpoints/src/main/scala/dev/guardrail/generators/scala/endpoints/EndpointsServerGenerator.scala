package dev.guardrail.generators.scala.endpoints

import _root_.io.swagger.v3.oas.models.Operation
import cats.data.NonEmptyList
import cats.Monad
import dev.guardrail.core.Tracker
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.terms.Responses
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.terms.server._
import dev.guardrail.terms.{ CollectionsLibTerms, SecurityScheme }
import dev.guardrail.Target

class EndpointsServerGenerator(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ServerTerms[ScalaLanguage, Target] {
  override implicit def MonadF: Monad[Target] = Target.targetInstances
  override def generateResponseDefinitions(
      responseClsName: String,
      responses: Responses[ScalaLanguage],
      protocolElems: List[StrictProtocolElems[ScalaLanguage]]
  ) =
    Target.raiseUserError("endpoints server generation is not currently supported")
  override def buildCustomExtractionFields(operation: Tracker[Operation], resourceName: List[String], customExtraction: Boolean) =
    Target.raiseUserError("endpoints server generation is not currently supported")
  override def buildTracingFields(operation: Tracker[Operation], resourceName: List[String], tracing: Boolean) =
    Target.raiseUserError("endpoints server generation is not currently supported")
  override def generateRoutes(
      tracing: Boolean,
      resourceName: String,
      handlerName: String,
      basePath: Option[String],
      routes: List[GenerateRouteMeta[ScalaLanguage]],
      protocolElems: List[StrictProtocolElems[ScalaLanguage]],
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
  ) =
    Target.raiseUserError("endpoints server generation is not currently supported")
  override def renderHandler(
      handlerName: String,
      methodSigs: List[scala.meta.Decl.Def],
      handlerDefinitions: List[scala.meta.Stat],
      responseDefinitions: List[scala.meta.Defn],
      customExtraction: Boolean
  ) =
    Target.raiseUserError("endpoints server generation is not currently supported")
  override def getExtraRouteParams(customExtraction: Boolean, tracing: Boolean) =
    Target.raiseUserError("endpoints server generation is not currently supported")
  override def generateSupportDefinitions(
      tracing: Boolean,
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
  ) =
    Target.raiseUserError("endpoints server generation is not currently supported")
  override def renderClass(
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
  override def getExtraImports(tracing: Boolean, supportPackage: NonEmptyList[String]) =
    Target.raiseUserError("endpoints server generation is not currently supported")
}
