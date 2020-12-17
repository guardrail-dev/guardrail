package com.twilio.guardrail
package generators

import cats.data.NonEmptyList
import com.twilio.guardrail.generators.Java._
import com.twilio.guardrail.generators.collections.{ JavaCollectionsGenerator, JavaVavrCollectionsGenerator }
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.terms.collections.{ CollectionsAbstraction, JavaStdLibCollections, JavaVavrCollections }
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }

object JavaModule extends AbstractModule[JavaLanguage] {
  def stdlib: (CollectionsLibTerms[JavaLanguage, Target], CollectionsAbstraction[JavaLanguage]) =
    (new JavaCollectionsGenerator.JavaCollectionsInterp, JavaStdLibCollections)
  def vavr: (CollectionsLibTerms[JavaLanguage, Target], CollectionsAbstraction[JavaLanguage]) =
    (new JavaVavrCollectionsGenerator.JavaVavrCollectionsInterp, JavaVavrCollections)

  def jackson(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): (
      ProtocolSupportTerms[JavaLanguage, Target],
      ModelProtocolTerms[JavaLanguage, Target],
      EnumProtocolTerms[JavaLanguage, Target],
      ArrayProtocolTerms[JavaLanguage, Target],
      PolyProtocolTerms[JavaLanguage, Target]
  ) = (
    JacksonGenerator.ProtocolSupportTermInterp,
    JacksonGenerator.ModelProtocolTermInterp,
    JacksonGenerator.EnumProtocolTermInterp,
    JacksonGenerator.ArrayProtocolTermInterp,
    JacksonGenerator.PolyProtocolTermInterp
  )

  def dropwizard(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  ) = (DropwizardServerGenerator.ServerTermInterp, DropwizardGenerator.FrameworkInterp)
  def spring(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  ) = (SpringMvcServerGenerator.ServerTermInterp, SpringMvcGenerator.FrameworkInterp)
  def asyncHttpClient(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): ClientTerms[JavaLanguage, Target] =
    AsyncHttpClientClientGenerator.ClientTermInterp

  def extract(modules: NonEmptyList[String]): Target[Framework[JavaLanguage, Target]] = {
    implicit val col = JavaStdLibCollections
    (for {
      (collections, col)                   <- popModule("collections", ("java-stdlib", stdlib), ("java-vavr", vavr))
      (protocol, model, enum, array, poly) <- popModule("json", ("jackson", jackson(collections, col)))
      client                               <- popModule("client", ("async-http-client", asyncHttpClient(collections, col)))
      (server, framework)                  <- popModule("server", ("dropwizard", dropwizard(collections, col)), ("spring-mvc", spring(collections, col)))
    } yield new Framework[JavaLanguage, Target] {
      def ArrayProtocolInterp: ArrayProtocolTerms[JavaLanguage, Target]     = array
      def ClientInterp: ClientTerms[JavaLanguage, Target]                   = client
      def EnumProtocolInterp: EnumProtocolTerms[JavaLanguage, Target]       = enum
      def FrameworkInterp: FrameworkTerms[JavaLanguage, Target]             = framework
      def ModelProtocolInterp: ModelProtocolTerms[JavaLanguage, Target]     = model
      def PolyProtocolInterp: PolyProtocolTerms[JavaLanguage, Target]       = poly
      def ProtocolSupportInterp: ProtocolSupportTerms[JavaLanguage, Target] = protocol
      def ServerInterp: ServerTerms[JavaLanguage, Target]                   = server
      def SwaggerInterp: SwaggerTerms[JavaLanguage, Target]                 = SwaggerGenerator[JavaLanguage]
      def LanguageInterp: LanguageTerms[JavaLanguage, Target]               = JavaGenerator.JavaInterp
      def CollectionsLibInterp: CollectionsLibTerms[JavaLanguage, Target]   = collections
    }).runA(modules.toList.toSet)
  }
}
