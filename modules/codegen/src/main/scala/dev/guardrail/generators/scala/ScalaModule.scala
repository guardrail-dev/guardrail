package dev.guardrail.generators.scala

import cats.data.NonEmptyList
import cats.implicits._

import dev.guardrail._
import dev.guardrail.generators._
import dev.guardrail.generators.spi.{ ClientGeneratorLoader, FrameworkGeneratorLoader, ProtocolGeneratorLoader, ServerGeneratorLoader }
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, ProtocolTerms, SwaggerTerms }

object ScalaModule extends AbstractModule[ScalaLanguage] {
  def circe(circeModelGenerator: String): Target[(String, ProtocolTerms[ScalaLanguage, Target])] = {
    val params = Set(circeModelGenerator)
    ProtocolGeneratorLoader
      .load[ScalaLanguage](params, MissingDependency("guardrail-scala-support"))
      .map((circeModelGenerator, _))
  }

  def circeJava8(circeModelGenerator: String): Target[(String, ProtocolTerms[ScalaLanguage, Target])] =
    for {
      (circeVersion, stockProtocolSupportInterp) <- circe(circeModelGenerator)
      newInterp = stockProtocolSupportInterp.copy(
        packageObjectImports = () =>
          stockProtocolSupportInterp.packageObjectImports().map { values =>
            import _root_.scala.meta._
            values :+ q"import io.circe.java8.time._"
          }
      )
    } yield (circeVersion, newInterp)

  def jackson: Target[(String, ProtocolTerms[ScalaLanguage, Target])] = {
    val params = Set("jackson")
    ProtocolGeneratorLoader
      .load[ScalaLanguage](params, MissingDependency("guardrail-scala-support"))
      .map(("jackson", _))
  }

  def akkaHttp(akkaHttpVersion: String, modelGeneratorType: String): Target[
    (
        ClientTerms[ScalaLanguage, Target],
        ServerTerms[ScalaLanguage, Target],
        FrameworkTerms[ScalaLanguage, Target]
    )
  ] = {
    val params = Set(akkaHttpVersion, modelGeneratorType)
    (
      ClientGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-akka-http")),
      ServerGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-akka-http")),
      FrameworkGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-akka-http"))
    ).mapN(Tuple3.apply)
  }

  def http4s(http4sVersion: String): Target[
    (
        ClientTerms[ScalaLanguage, Target],
        ServerTerms[ScalaLanguage, Target],
        FrameworkTerms[ScalaLanguage, Target]
    )
  ] = {
    val params = Set(http4sVersion)
    (
      ClientGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-http4s")),
      ServerGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-http4s")),
      FrameworkGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-http4s"))
    ).mapN(Tuple3.apply)
  }

  def dropwizard: Target[
    (
        ClientTerms[ScalaLanguage, Target],
        ServerTerms[ScalaLanguage, Target],
        FrameworkTerms[ScalaLanguage, Target]
    )
  ] = {
    val params = Set("dropwizard")
    (
      ClientGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-dropwizard")),
      ServerGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-dropwizard")),
      FrameworkGeneratorLoader.load[ScalaLanguage](params, MissingDependency("guardrail-scala-dropwizard"))
    ).mapN(Tuple3.apply)
  }

  def extract(modules: NonEmptyList[String]): Target[Framework[ScalaLanguage, Target]] =
    (for {
      (modelGeneratorType, protocol) <- popModule(
        "json",
        ("circe-java8", circeJava8("circe-v0.11")),
        ("circe-v0.11", circe("circe-v0.11")),
        ("circe-v0.12", circe("circe-v0.12")),
        ("circe", circe("circe-v0.12")),
        ("jackson", jackson)
      )
      (client, server, framework) <- popModule(
        "framework",
        ("akka-http", akkaHttp("akka-http-v10.2", modelGeneratorType)),
        ("akka-http-v10.1", akkaHttp("akka-http-v10.1", modelGeneratorType)),
        ("akka-http-v10.2", akkaHttp("akka-http-v10.2", modelGeneratorType)),
        ("http4s", http4s("http4s-v0.23")),
        ("http4s-v0.22", http4s("http4s-v0.22")),
        ("http4s-v0.23", http4s("http4s-v0.23")),
        ("dropwizard", dropwizard)
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
      def CollectionsLibInterp: CollectionsLibTerms[ScalaLanguage, Target] = ScalaCollectionsGenerator()
    }).runA(
      modules
        .map {
          case oldv @ "circe-0.11" =>
            val newv = "circe-v0.11"
            println(s"Deprecation: ${oldv} has been renamed to ${newv} for consistency")
            newv
          case other => other
        }
        .toList
        .toSet
    )
}
