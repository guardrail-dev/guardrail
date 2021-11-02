package dev.guardrail.generators.java

import cats.data.NonEmptyList

import dev.guardrail._
import dev.guardrail.generators.java.asyncHttpClient.AsyncHttpClientClientGenerator
import dev.guardrail.generators.{ AbstractModule, Framework, SwaggerGenerator }
import dev.guardrail.generators.java.jackson.JacksonGenerator
import dev.guardrail.generators.java.dropwizard.{ DropwizardGenerator, DropwizardServerGenerator }
import dev.guardrail.generators.java.springMvc.{ SpringMvcGenerator, SpringMvcServerGenerator }
import dev.guardrail.generators.collections.{ JavaCollectionsGenerator, JavaVavrCollectionsGenerator }
import dev.guardrail.terms.client.ClientTerms
import dev.guardrail.terms.server.ServerTerms
import dev.guardrail.terms.collections.{ CollectionsAbstraction, JavaStdLibCollections, JavaVavrCollections }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, ProtocolTerms, SwaggerTerms }

object JavaModule extends AbstractModule[JavaLanguage] {
  private def catchClassNotFound[A](value: => A, error: => MissingDependency): Target[A] =
    try {
      Target.pure(value)
    } catch {
      case _: java.lang.NoClassDefFoundError =>
        Target.raiseError(error)
    }

  def stdlib: (CollectionsLibTerms[JavaLanguage, Target], CollectionsAbstraction[JavaLanguage]) =
    (JavaCollectionsGenerator(), JavaStdLibCollections)
  def vavr: (CollectionsLibTerms[JavaLanguage, Target], CollectionsAbstraction[JavaLanguage]) =
    (JavaVavrCollectionsGenerator(), JavaVavrCollections)

  def jackson(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): ProtocolTerms[JavaLanguage, Target] =
    JacksonGenerator()
  def dropwizard(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  ) = (DropwizardServerGenerator(), DropwizardGenerator())
  def spring(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  ) = (SpringMvcServerGenerator(), SpringMvcGenerator())
  def asyncHttpClient(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): ClientTerms[JavaLanguage, Target] =
    AsyncHttpClientClientGenerator()

  def extract(modules: NonEmptyList[String]): Target[Framework[JavaLanguage, Target]] = {
    implicit val col = JavaStdLibCollections
    (for {
      (collections, col) <- popModule(
        "collections",
        ("java-stdlib", catchClassNotFound(stdlib, MissingDependency("guardrail-java-support"))),
        ("java-vavr", catchClassNotFound(vavr, MissingDependency("guardrail-java-support")))
      )
      protocol <- popModule(
        "json",
        ("jackson", catchClassNotFound(jackson(collections, col), MissingDependency("guardrail-java-support")))
      )
      client <- popModule(
        "client",
        ("async-http-client", catchClassNotFound(asyncHttpClient(collections, col), MissingDependency("guardrail-java-async-http")))
      )
      (server, framework) <- popModule(
        "server",
        ("dropwizard", catchClassNotFound(dropwizard(collections, col), MissingDependency("guardrail-java-dropwizard"))),
        ("spring-mvc", catchClassNotFound(spring(collections, col), MissingDependency("guardrail-java-spring-mvc")))
      )
    } yield new Framework[JavaLanguage, Target] {
      def ClientInterp: ClientTerms[JavaLanguage, Target]                 = client
      def FrameworkInterp: FrameworkTerms[JavaLanguage, Target]           = framework
      def ProtocolInterp: ProtocolTerms[JavaLanguage, Target]             = protocol
      def ServerInterp: ServerTerms[JavaLanguage, Target]                 = server
      def SwaggerInterp: SwaggerTerms[JavaLanguage, Target]               = SwaggerGenerator[JavaLanguage]()
      def LanguageInterp: LanguageTerms[JavaLanguage, Target]             = JavaGenerator()
      def CollectionsLibInterp: CollectionsLibTerms[JavaLanguage, Target] = collections
    }).runA(modules.toList.toSet)
  }
}
