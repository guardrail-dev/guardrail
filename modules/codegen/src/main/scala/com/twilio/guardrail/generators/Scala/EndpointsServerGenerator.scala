package com.twilio.guardrail.generators.Scala

import _root_.io.swagger.v3.oas.models.Operation
import cats.Monad
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.Responses
import com.twilio.guardrail.protocol.terms.server._
import com.twilio.guardrail.terms.SecurityScheme
import com.twilio.guardrail.{ StrictProtocolElems, Target }

object EndpointsServerGenerator {
  object ServerTermInterp extends ServerTerms[ScalaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def generateResponseDefinitions(responseClsName: String, responses: Responses[ScalaLanguage], protocolElems: List[StrictProtocolElems[ScalaLanguage]]) =
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
        responseDefinitions: List[scala.meta.Defn]
    ) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def getExtraRouteParams(tracing: Boolean) =
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
        supportDefinitions: List[scala.meta.Defn]
    ) =
      Target.raiseUserError("endpoints server generation is not currently supported")
    def getExtraImports(tracing: Boolean, supportPackage: List[String]) =
      Target.raiseUserError("endpoints server generation is not currently supported")
  }
}
