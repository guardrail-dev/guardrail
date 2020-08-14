package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.ScalaLanguage
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.generators.Scala._
import com.twilio.guardrail.generators.Scala.model.{ CirceModelGenerator, JacksonModelGenerator, ModelGeneratorType }
import com.twilio.guardrail.terms.{ LanguageTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms

object ScalaModule extends AbstractModule[ScalaLanguage] {
  def circe(circeModelGenerator: CirceModelGenerator): (
      ProtocolSupportTerms[ScalaLanguage, Target],
      ModelProtocolTerms[ScalaLanguage, Target],
      EnumProtocolTerms[ScalaLanguage, Target],
      ArrayProtocolTerms[ScalaLanguage, Target],
      PolyProtocolTerms[ScalaLanguage, Target]
  ) = (
    CirceProtocolGenerator.ProtocolSupportTermInterp,
    new CirceProtocolGenerator.ModelProtocolTermInterp(circeModelGenerator),
    CirceProtocolGenerator.EnumProtocolTermInterp,
    CirceProtocolGenerator.ArrayProtocolTermInterp,
    CirceProtocolGenerator.PolyProtocolTermInterp
  )

  def circeJava8(circeModelGenerator: CirceModelGenerator): (
      ProtocolSupportTerms[ScalaLanguage, Target],
      ModelProtocolTerms[ScalaLanguage, Target],
      EnumProtocolTerms[ScalaLanguage, Target],
      ArrayProtocolTerms[ScalaLanguage, Target],
      PolyProtocolTerms[ScalaLanguage, Target]
  ) = (
    CirceProtocolGenerator.ProtocolSupportTermInterp.copy(newPackageObjectImports =
      () =>
        CirceProtocolGenerator.ProtocolSupportTermInterp.packageObjectImports().map { values =>
          import scala.meta._
          values :+ q"import io.circe.java8.time._"
        }
    ),
    new CirceProtocolGenerator.ModelProtocolTermInterp(circeModelGenerator),
    CirceProtocolGenerator.EnumProtocolTermInterp,
    CirceProtocolGenerator.ArrayProtocolTermInterp,
    CirceProtocolGenerator.PolyProtocolTermInterp
  )

  def jackson: (
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

  def akkaHttp(modelGeneratorType: ModelGeneratorType): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    new AkkaHttpClientGenerator.ClientTermInterp(modelGeneratorType),
    new AkkaHttpServerGenerator.ServerTermInterp(modelGeneratorType),
    new AkkaHttpGenerator.FrameworkInterp(modelGeneratorType)
  )

  def endpoints(modelGeneratorType: ModelGeneratorType): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    EndpointsClientGenerator.ClientTermInterp,
    EndpointsServerGenerator.ServerTermInterp,
    EndpointsGenerator.FrameworkInterp
  )

  def http4s: (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    Http4sClientGenerator.ClientTermInterp,
    Http4sServerGenerator.ServerTermInterp,
    Http4sGenerator.FrameworkInterp
  )

  def dropwizard: (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (
    DropwizardClientGenerator.ClientTermInterp,
    DropwizardServerGenerator.ServerTermInterp,
    DropwizardGenerator.FrameworkInterp
  )

  def extract(modules: NonEmptyList[String]): Target[Framework[ScalaLanguage, Target]] =
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
    }).runA(modules.toList.toSet)
}
