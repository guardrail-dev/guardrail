package dev.guardrail.generators.scala.dropwizard

import cats.Monad
import cats.data.NonEmptyList
import java.net.URI
import scala.meta.{ Defn, Import, Term }
import scala.reflect.runtime.universe.typeTag

import dev.guardrail.core.SupportDefinition
import dev.guardrail.generators.{ LanguageParameters, RenderedClientOperation }
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.spi.{ ClientGeneratorLoader, ModuleLoadResult }
import dev.guardrail.terms.Responses
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.protocol.{ StaticDefns, StrictProtocolElems }
import dev.guardrail.terms.{ RouteMeta, SecurityScheme }
import dev.guardrail.{ RuntimeFailure, Target }

class DropwizardClientGeneratorLoader extends ClientGeneratorLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  val apply   = ModuleLoadResult.buildFrom(ModuleLoadResult.extract(DropwizardVersion.unapply))(_ => DropwizardClientGenerator())
}

object DropwizardClientGenerator {
  def apply(): ClientTerms[ScalaLanguage, Target] =
    new DropwizardClientGenerator
}

class DropwizardClientGenerator private extends ClientTerms[ScalaLanguage, Target] {
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
