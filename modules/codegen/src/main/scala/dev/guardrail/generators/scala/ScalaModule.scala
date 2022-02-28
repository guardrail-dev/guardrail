package dev.guardrail.generators.scala

import cats.data.NonEmptyList

import dev.guardrail._
import dev.guardrail.generators._
import dev.guardrail.generators.scala.akkaHttp.{ AkkaHttpClientGenerator, AkkaHttpGenerator, AkkaHttpServerGenerator }
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator
import dev.guardrail.generators.scala.dropwizard.{ DropwizardClientGenerator, DropwizardGenerator, DropwizardServerGenerator }
import dev.guardrail.generators.scala.http4s.{ Http4sClientGenerator, Http4sGenerator, Http4sServerGenerator, Http4sVersion }
import dev.guardrail.generators.scala.jackson.JacksonProtocolGenerator
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, ProtocolTerms, SwaggerTerms }

object ScalaModule extends AbstractModule[ScalaLanguage] {
  private def catchClassNotFound[A](value: => A, error: => MissingDependency): Target[A] =
    try
      Target.pure(value)
    catch {
      case _: _root_.java.lang.NoClassDefFoundError =>
        Target.raiseError(error)
    }

  def circe(circeModelGenerator: CirceModelGenerator)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ProtocolTerms[ScalaLanguage, Target] =
    CirceProtocolGenerator(circeModelGenerator)

  def circeJava8(circeModelGenerator: CirceModelGenerator)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ProtocolTerms[ScalaLanguage, Target] = {
    val stockProtocolSupportInterp = CirceProtocolGenerator(circeModelGenerator)
    stockProtocolSupportInterp.copy(
      packageObjectImports = () =>
        stockProtocolSupportInterp.packageObjectImports().map { values =>
          import _root_.scala.meta._
          values :+ q"import io.circe.java8.time._"
        }
    )
  }

  def jackson(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): ProtocolTerms[ScalaLanguage, Target] = JacksonProtocolGenerator.apply

  def akkaHttp(modelGeneratorType: ModelGeneratorType)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    AkkaHttpClientGenerator(modelGeneratorType),
    AkkaHttpServerGenerator(AkkaHttpVersion.V10_2, modelGeneratorType),
    AkkaHttpGenerator(AkkaHttpVersion.V10_2, modelGeneratorType)
  )

  def akkaHttpV10_1(modelGeneratorType: ModelGeneratorType)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    AkkaHttpClientGenerator(modelGeneratorType),
    AkkaHttpServerGenerator(AkkaHttpVersion.V10_1, modelGeneratorType),
    AkkaHttpGenerator(AkkaHttpVersion.V10_1, modelGeneratorType)
  )

  def http4s(http4sVersion: Http4sVersion)(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    Http4sClientGenerator(),
    Http4sServerGenerator(http4sVersion),
    Http4sGenerator()
  )

  def dropwizard(implicit Cl: CollectionsLibTerms[ScalaLanguage, Target]): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    DropwizardClientGenerator(),
    DropwizardServerGenerator(),
    DropwizardGenerator()
  )

  def extract(modules: NonEmptyList[String]): Target[Framework[ScalaLanguage, Target]] = {
    implicit val collections = ScalaCollectionsGenerator()
    (for {
      (modelGeneratorType, protocol) <- popModule(
        "json",
        ("circe-java8", catchClassNotFound((CirceModelGenerator.V011, circeJava8(CirceModelGenerator.V011)), MissingDependency("guardrail-scala-support"))),
        ("circe-0.11", catchClassNotFound((CirceModelGenerator.V011, circe(CirceModelGenerator.V011)), MissingDependency("guardrail-scala-support"))),
        ("circe", catchClassNotFound((CirceModelGenerator.V012, circe(CirceModelGenerator.V012)), MissingDependency("guardrail-scala-support"))),
        ("jackson", catchClassNotFound((JacksonModelGenerator, jackson), MissingDependency("guardrail-scala-support")))
      )
      (client, server, framework) <- popModule(
        "framework",
        ("akka-http", catchClassNotFound(akkaHttp(modelGeneratorType), MissingDependency("guardrail-scala-akka-http"))),
        ("akka-http-v10.1", catchClassNotFound(akkaHttpV10_1(modelGeneratorType), MissingDependency("guardrail-scala-akka-http"))),
        ("http4s", catchClassNotFound(http4s(Http4sVersion.V0_23), MissingDependency("guardrail-scala-http4s"))),
        ("http4s-v0.23", catchClassNotFound(http4s(Http4sVersion.V0_23), MissingDependency("guardrail-scala-http4s"))),
        ("http4s-v0.22", catchClassNotFound(http4s(Http4sVersion.V0_22), MissingDependency("guardrail-scala-http4s"))),
        ("dropwizard", catchClassNotFound(dropwizard, MissingDependency("guardrail-scala-dropwizard")))
      )
      // parser             =  or interpFramework
      // codegenApplication = ScalaGenerator or parser
    } yield new Framework[ScalaLanguage, Target] {
      def ClientInterp: ClientTerms[ScalaLanguage, Target]                 = client
      def FrameworkInterp: FrameworkTerms[ScalaLanguage, Target]           = framework
      def ProtocolInterp: ProtocolTerms[ScalaLanguage, Target]             = protocol
      def ServerInterp: ServerTerms[ScalaLanguage, Target]                 = server
      def SwaggerInterp: SwaggerTerms[ScalaLanguage, Target]               = SwaggerGenerator[ScalaLanguage]()
      def LanguageInterp: LanguageTerms[ScalaLanguage, Target]             = ScalaGenerator()
      def CollectionsLibInterp: CollectionsLibTerms[ScalaLanguage, Target] = collections
    }).runA(modules.toList.toSet)
  }
}
