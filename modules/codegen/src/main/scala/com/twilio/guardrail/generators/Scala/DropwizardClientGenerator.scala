package com.twilio.guardrail.generators.Scala

import cats.Monad
import cats.data.NonEmptyList
import com.twilio.guardrail.{ RenderedClientOperation, RuntimeFailure, StaticDefns, StrictProtocolElems, SupportDefinition, Target }
import com.twilio.guardrail.generators.LanguageParameters
import com.twilio.guardrail.languages.ScalaLanguage
import com.twilio.guardrail.protocol.terms.Responses
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.terms.{ RouteMeta, SecurityScheme }
import java.net.URI
import scala.meta.{ Defn, Import, Term }

object DropwizardClientGenerator {
  object ClientTermInterp extends ClientTerms[ScalaLanguage, Target] {
    override def MonadF: Monad[Target]                                   = Target.targetInstances
    override def getImports(tracing: Boolean): Target[List[Import]]      = Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
    override def getExtraImports(tracing: Boolean): Target[List[Import]] = Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
    override def clientClsArgs(tracingName: Option[String], serverUrls: Option[NonEmptyList[URI]], tracing: Boolean): Target[List[List[Term.Param]]] =
      Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
    override def generateResponseDefinitions(
        responseClsName: String,
        responses: Responses[ScalaLanguage],
        protocolElems: List[StrictProtocolElems[ScalaLanguage]]
    ): Target[List[Defn]] = Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
    override def generateSupportDefinitions(
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]]
    ): Target[List[SupportDefinition[ScalaLanguage]]] = Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
    override def buildStaticDefns(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        ctorArgs: List[List[Term.Param]],
        tracing: Boolean
    ): Target[StaticDefns[ScalaLanguage]] = Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
    override def generateClientOperation(
        className: List[String],
        responseClsName: String,
        tracing: Boolean,
        securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
        parameters: LanguageParameters[ScalaLanguage]
    )(route: RouteMeta, methodName: String, responses: Responses[ScalaLanguage]): Target[RenderedClientOperation[ScalaLanguage]] =
      Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
    override def buildClient(
        clientName: String,
        tracingName: Option[String],
        serverUrls: Option[NonEmptyList[URI]],
        basePath: Option[String],
        ctorArgs: List[List[Term.Param]],
        clientCalls: List[Defn],
        supportDefinitions: List[Defn],
        tracing: Boolean
    ): Target[NonEmptyList[Either[Defn.Trait, Defn.Class]]] = Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
  }
}
