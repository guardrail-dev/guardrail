package dev.guardrail
package generators

import cats.data.NonEmptyList
import dev.guardrail.generators.Scala._
import dev.guardrail.generators.Scala.model.{ CirceModelGenerator, JacksonModelGenerator, ModelGeneratorType }
import dev.guardrail.generators.collections.ScalaCollectionsGenerator
import dev.guardrail.languages.ScalaLanguage
import dev.guardrail.protocol.terms.client.ClientTerms
import dev.guardrail.protocol.terms.protocol._
import dev.guardrail.protocol.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }

object ScalaModule extends AbstractModule[ScalaLanguage] {
  def circe(circeModelGenerator: CirceModelGenerator)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ProtocolSupportTerms[ScalaLanguage, Target],
      ModelProtocolTerms[ScalaLanguage, Target],
      EnumProtocolTerms[ScalaLanguage, Target],
      ArrayProtocolTerms[ScalaLanguage, Target],
      PolyProtocolTerms[ScalaLanguage, Target]
  ) = (
    CirceProtocolGenerator.ProtocolSupportTermInterp,
    CirceProtocolGenerator.ModelProtocolTermInterp(circeModelGenerator),
    CirceProtocolGenerator.EnumProtocolTermInterp,
    CirceProtocolGenerator.ArrayProtocolTermInterp,
    CirceProtocolGenerator.PolyProtocolTermInterp
  )

  def circeJava8(circeModelGenerator: CirceModelGenerator)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ProtocolSupportTerms[ScalaLanguage, Target],
      ModelProtocolTerms[ScalaLanguage, Target],
      EnumProtocolTerms[ScalaLanguage, Target],
      ArrayProtocolTerms[ScalaLanguage, Target],
      PolyProtocolTerms[ScalaLanguage, Target]
  ) = {
    val stockProtocolSupportInterp = CirceProtocolGenerator.ProtocolSupportTermInterp
    val protocolSupportInterp = stockProtocolSupportInterp.copy(
      newPackageObjectImports = () =>
        stockProtocolSupportInterp.packageObjectImports().map { values =>
          import scala.meta._
          values :+ q"import io.circe.java8.time._"
        }
    )
    (
      protocolSupportInterp,
      CirceProtocolGenerator.ModelProtocolTermInterp(circeModelGenerator),
      CirceProtocolGenerator.EnumProtocolTermInterp,
      CirceProtocolGenerator.ArrayProtocolTermInterp,
      CirceProtocolGenerator.PolyProtocolTermInterp
    )
  }

  def jackson(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ProtocolSupportTerms[ScalaLanguage, Target],
      ModelProtocolTerms[ScalaLanguage, Target],
      EnumProtocolTerms[ScalaLanguage, Target],
      ArrayProtocolTerms[ScalaLanguage, Target],
      PolyProtocolTerms[ScalaLanguage, Target]
  ) = (
    JacksonProtocolGenerator.ProtocolSupportTermInterp,
    JacksonProtocolGenerator.ModelProtocolTermInterp,
    JacksonProtocolGenerator.EnumProtocolTermInterp,
    JacksonProtocolGenerator.ArrayProtocolTermInterp,
    JacksonProtocolGenerator.PolyProtocolTermInterp
  )

  def akkaHttp(modelGeneratorType: ModelGeneratorType)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    AkkaHttpClientGenerator.ClientTermInterp(modelGeneratorType),
    AkkaHttpServerGenerator.ServerTermInterp(modelGeneratorType),
    AkkaHttpGenerator.FrameworkInterp(modelGeneratorType)
  )

  def endpoints(modelGeneratorType: ModelGeneratorType)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    EndpointsClientGenerator.ClientTermInterp,
    EndpointsServerGenerator.ServerTermInterp,
    EndpointsGenerator.FrameworkInterp
  )

  def http4s(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    Http4sClientGenerator.ClientTermInterp,
    Http4sServerGenerator.ServerTermInterp,
    Http4sGenerator.FrameworkInterp
  )

  def dropwizard(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    DropwizardClientGenerator.ClientTermInterp,
    DropwizardServerGenerator.ServerTermInterp,
    DropwizardGenerator.FrameworkInterp
  )

  def extract(modules: NonEmptyList[String]): Target[Framework[ScalaLanguage, Target]] = {
    implicit val collections = ScalaCollectionsGenerator.ScalaCollectionsInterp
    (for {
      (modelGeneratorType, (protocol, model, enum, array, poly)) <- popModule(
        "json",
        ("circe-java8", (CirceModelGenerator.V011, circeJava8(CirceModelGenerator.V011))),
        ("circe-0.11", (CirceModelGenerator.V011, circe(CirceModelGenerator.V011))),
        ("circe", (CirceModelGenerator.V012, circe(CirceModelGenerator.V012))),
        ("jackson", (JacksonModelGenerator, jackson))
      )
      (client, server, framework) <- popModule(
        "framework",
        ("akka-http", akkaHttp(modelGeneratorType)),
        ("http4s", http4s),
        ("endpoints", endpoints(modelGeneratorType)),
        ("dropwizard", dropwizard)
      )
      // parser             =  or interpFramework
      // codegenApplication = ScalaGenerator.ScalaInterp or parser
    } yield new Framework[ScalaLanguage, Target] {
      def ArrayProtocolInterp: ArrayProtocolTerms[ScalaLanguage, Target]     = array
      def ClientInterp: ClientTerms[ScalaLanguage, Target]                   = client
      def EnumProtocolInterp: EnumProtocolTerms[ScalaLanguage, Target]       = enum
      def FrameworkInterp: FrameworkTerms[ScalaLanguage, Target]             = framework
      def ModelProtocolInterp: ModelProtocolTerms[ScalaLanguage, Target]     = model
      def PolyProtocolInterp: PolyProtocolTerms[ScalaLanguage, Target]       = poly
      def ProtocolSupportInterp: ProtocolSupportTerms[ScalaLanguage, Target] = protocol
      def ServerInterp: ServerTerms[ScalaLanguage, Target]                   = server
      def SwaggerInterp: SwaggerTerms[ScalaLanguage, Target]                 = SwaggerGenerator[ScalaLanguage]
      def LanguageInterp: LanguageTerms[ScalaLanguage, Target]               = ScalaGenerator.ScalaInterp
      def CollectionsLibInterp: CollectionsLibTerms[ScalaLanguage, Target]   = collections
    }).runA(modules.toList.toSet)
  }
}
