package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.generators.Java.JacksonGenerator
import com.twilio.guardrail.generators.Java.AsyncHttpClientClientGenerator
import com.twilio.guardrail.generators.Java.DropwizardGenerator
import com.twilio.guardrail.generators.Java.SpringMvcGenerator
import com.twilio.guardrail.generators.Java.DropwizardServerGenerator
import com.twilio.guardrail.generators.Java.SpringMvcServerGenerator
import cats.data.NonEmptyList
import com.twilio.guardrail.generators.collections.JavaCollectionsGenerator
import com.twilio.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.terms.{ CollectionsLibTerms, LanguageTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms

object JavaModule extends AbstractModule[JavaLanguage] {
  def jackson: (
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

  def dropwizard: (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  ) = (DropwizardServerGenerator.ServerTermInterp, DropwizardGenerator.FrameworkInterp)
  def spring: (
      ServerTerms[JavaLanguage, Target],
      FrameworkTerms[JavaLanguage, Target]
  )                                                      = (SpringMvcServerGenerator.ServerTermInterp, SpringMvcGenerator.FrameworkInterp)
  def asyncHttpClient: ClientTerms[JavaLanguage, Target] = AsyncHttpClientClientGenerator.ClientTermInterp

  def extract(modules: NonEmptyList[String]): Target[Framework[JavaLanguage, Target]] =
    (for {
      (protocol, model, enum, array, poly) <- popModule("json", ("jackson", jackson))
      client                               <- popModule("client", ("async-http-client", asyncHttpClient))
      (server, framework)                  <- popModule("server", ("dropwizard", dropwizard), ("spring-mvc", spring))
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
      def CollectionsLibInterp: CollectionsLibTerms[JavaLanguage, Target]   = JavaCollectionsGenerator.JavaCollectionsInterp
    }).runA(modules.toList.toSet)
}
