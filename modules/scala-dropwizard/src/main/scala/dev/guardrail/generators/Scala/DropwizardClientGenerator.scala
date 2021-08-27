package dev.guardrail.generators.Scala

import cats.Monad
import cats.data.NonEmptyList
import dev.guardrail.{ RenderedClientOperation, RuntimeFailure, StaticDefns, StrictProtocolElems, SupportDefinition, Target }
import dev.guardrail.generators.LanguageParameters
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.Responses
import dev.guardrail.protocol.terms.client.ClientTerms
import dev.guardrail.terms.{ CollectionsLibTerms, RouteMeta, SecurityScheme }
import java.net.URI

import scala.meta.{ Defn, Import, Term }

object DropwizardClientGenerator {
  def ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ClientTerms[ScalaLanguage, Target] = new ClientTermInterp
  class ClientTermInterp(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]) extends ClientTerms[ScalaLanguage, Target] {
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
