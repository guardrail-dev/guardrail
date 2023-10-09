package dev.guardrail.generators.scala.dropwizard

import _root_.io.swagger.v3.oas.models.Components
import cats.Monad
import cats.data.NonEmptyList
import dev.guardrail.Context
import dev.guardrail.RuntimeFailure
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.generators.Clients
import dev.guardrail.generators.scala.ScalaLanguage
import dev.guardrail.generators.spi.ClientGeneratorLoader
import dev.guardrail.generators.spi.ModuleLoadResult
import dev.guardrail.terms._
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.StrictProtocolElems

import java.net.URI
import scala.reflect.runtime.universe.typeTag

class DropwizardClientGeneratorLoader extends ClientGeneratorLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  val apply   = ModuleLoadResult.forProduct1(ClientGeneratorLoader.label -> Seq(DropwizardVersion.mapping))(_ => DropwizardClientGenerator())
}

object DropwizardClientGenerator {
  def apply(): ClientTerms[ScalaLanguage, Target] =
    new DropwizardClientGenerator
}

class DropwizardClientGenerator private extends ClientTerms[ScalaLanguage, Target] {
  override def MonadF: Monad[Target] = Target.targetInstances

  override def fromSpec(context: Context, frameworkImports: List[ScalaLanguage#Import])(
      serverUrls: Option[NonEmptyList[URI]],
      basePath: Option[String],
      groupedRoutes: List[(List[String], List[RouteMeta])]
  )(
      protocolElems: List[StrictProtocolElems[ScalaLanguage]],
      securitySchemes: Map[String, SecurityScheme[ScalaLanguage]],
      components: Tracker[Option[Components]]
  )(implicit
      Fw: FrameworkTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: SwaggerTerms[ScalaLanguage, Target]
  ): Target[Clients[ScalaLanguage]] =
    Target.raiseError(RuntimeFailure("Dropwizard Scala clients are not yet supported"))
}
