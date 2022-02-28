package dev.guardrail.generators.java

import cats.data.NonEmptyList
import cats.implicits._

import dev.guardrail._
import dev.guardrail.generators.{ AbstractModule, Framework, SwaggerGenerator }
import dev.guardrail.generators.spi.{
  ClientGeneratorLoader,
  CollectionsGeneratorLoader,
  FrameworkGeneratorLoader,
  ProtocolGeneratorLoader,
  ServerGeneratorLoader
}
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, ProtocolTerms, SwaggerTerms }

object JavaModule extends AbstractModule[JavaLanguage] {
  private def catchClassNotFound[A](value: => A, error: => MissingDependency): Target[A] =
    try
      Target.pure(value)
    catch {
      case _: java.lang.NoClassDefFoundError =>
        Target.raiseError(error)
    }

  def stdlib: Target[(String, CollectionsLibTerms[JavaLanguage, Target])] = {
    val params = Set("java-stdlib")
    CollectionsGeneratorLoader
      .load[JavaLanguage](params, MissingDependency("guardrail-java-support"))
      .map(("java-stdlib", _))
  }
  def vavr: Target[(String, CollectionsLibTerms[JavaLanguage, Target])] = {
    val params = Set("java-vavr")
    CollectionsGeneratorLoader
      .load[JavaLanguage](params, MissingDependency("guardrail-java-support"))
      .map(("java-vavr", _))
  }

  def jackson(collections: String): Target[ProtocolTerms[JavaLanguage, Target]] = {
    val params = Set("jackson", collections)
    ProtocolGeneratorLoader.load[JavaLanguage](params, MissingDependency("guardrail-java-support"))
  }
  def dropwizard(collections: String): Target[
    (
        ServerTerms[JavaLanguage, Target],
        FrameworkTerms[JavaLanguage, Target]
    )
  ] = {
    val params = Set("dropwizard", collections)
    (
      ServerGeneratorLoader.load[JavaLanguage](params, MissingDependency("guardrail-java-dropwizard")),
      FrameworkGeneratorLoader.load[JavaLanguage](params, MissingDependency("guardrail-java-dropwizard"))
    ).mapN(Tuple2.apply)
  }
  def spring(collections: String): Target[
    (
        ServerTerms[JavaLanguage, Target],
        FrameworkTerms[JavaLanguage, Target]
    )
  ] = {
    val params = Set("spring-mvc", collections)
    (
      ServerGeneratorLoader.load[JavaLanguage](params, MissingDependency("guardrail-java-spring-mvc")),
      FrameworkGeneratorLoader.load[JavaLanguage](params, MissingDependency("guardrail-java-spring-mvc"))
    ).mapN(Tuple2.apply)
  }
  def asyncHttpClient(collections: String): Target[ClientTerms[JavaLanguage, Target]] = {
    val params = Set("async-http-client", collections)
    ClientGeneratorLoader.load[JavaLanguage](params, MissingDependency("guardrail-java-async-http"))
  }

  def extract(modules: NonEmptyList[String]): Target[Framework[JavaLanguage, Target]] =
    (for {
      (collections, collectionsImpl) <- popModule(
        "collections",
        ("java-stdlib", stdlib),
        ("java-vavr", vavr)
      )
      protocol <- popModule(
        "json",
        ("jackson", jackson(collections))
      )
      client <- popModule(
        "client",
        ("async-http-client", asyncHttpClient(collections))
      )
      (server, framework) <- popModule(
        "server",
        ("dropwizard", dropwizard(collections)),
        ("spring-mvc", spring(collections))
      )
    } yield new Framework[JavaLanguage, Target] {
      def ClientInterp: ClientTerms[JavaLanguage, Target]                 = client
      def FrameworkInterp: FrameworkTerms[JavaLanguage, Target]           = framework
      def ProtocolInterp: ProtocolTerms[JavaLanguage, Target]             = protocol
      def ServerInterp: ServerTerms[JavaLanguage, Target]                 = server
      def SwaggerInterp: SwaggerTerms[JavaLanguage, Target]               = SwaggerGenerator[JavaLanguage]()
      def LanguageInterp: LanguageTerms[JavaLanguage, Target]             = JavaGenerator()
      def CollectionsLibInterp: CollectionsLibTerms[JavaLanguage, Target] = collectionsImpl
    }).runA(modules.toList.toSet)
}
