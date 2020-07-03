package com.twilio.guardrail
package generators

import cats.data.NonEmptyList
import com.twilio.guardrail.generators.Java.collectionslib.{ CollectionsLibType, JavaStdLibCollections, JavaVavrCollections }
import com.twilio.guardrail.generators.Java._
import com.twilio.guardrail.generators.collections.{ JavaCollectionsGenerator, JavaVavrCollectionsGenerator }
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }

object JavaModule extends AbstractModule[JavaLanguage] {
  def jackson(implicit Cl: CollectionsLibTerms[JavaLanguage, Target] with CollectionsLibType): (
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

  def dropwizard(implicit Cl: CollectionsLibTerms[JavaLanguage, Target] with CollectionsLibType): (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  ) = (DropwizardServerGenerator.ServerTermInterp, DropwizardGenerator.FrameworkInterp)
  def spring(implicit Cl: CollectionsLibTerms[JavaLanguage, Target] with CollectionsLibType): (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  ) = (SpringMvcServerGenerator.ServerTermInterp, SpringMvcGenerator.FrameworkInterp)
  def asyncHttpClient(implicit Cl: CollectionsLibTerms[JavaLanguage, Target] with CollectionsLibType): ClientTerms[JavaLanguage, Target] =
    AsyncHttpClientClientGenerator.ClientTermInterp

  def extract(modules: NonEmptyList[String]): Target[Framework[JavaLanguage, Target]] =
    (for {
      collections <- popModule(
        "collections",
        Some(new JavaCollectionsGenerator.JavaCollectionsInterp with JavaStdLibCollections),
        ("java-stdlib", new JavaCollectionsGenerator.JavaCollectionsInterp with JavaStdLibCollections),
        ("java-vavr", new JavaVavrCollectionsGenerator.JavaVavrCollectionsInterp with JavaVavrCollections)
      )
      (protocol, model, enum, array, poly) <- popModule("json", Some(jackson(collections)), ("jackson", jackson(collections)))
      client                               <- popModule("client", ("async-http-client", asyncHttpClient(collections)))
      (server, framework)                  <- popModule("server", ("dropwizard", dropwizard(collections)), ("spring-mvc", spring(collections)))
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
