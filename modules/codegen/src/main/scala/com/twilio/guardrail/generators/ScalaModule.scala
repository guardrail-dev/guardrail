package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.ScalaLanguage
import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.circe.CirceVersion

import com.twilio.guardrail.protocol.terms.protocol.{ ArrayProtocolTerms, EnumProtocolTerms, ModelProtocolTerms, PolyProtocolTerms, ProtocolSupportTerms }
import com.twilio.guardrail.protocol.terms.client.ClientTerms
import com.twilio.guardrail.protocol.terms.server.ServerTerms
import com.twilio.guardrail.terms.{ LanguageTerms, SwaggerTerms }
import com.twilio.guardrail.terms.framework.FrameworkTerms

object ScalaModule extends AbstractModule[ScalaLanguage] {
  def circe(circeVersion: CirceVersion): (
      ProtocolSupportTerms[ScalaLanguage, Target],
      ModelProtocolTerms[ScalaLanguage, Target],
      EnumProtocolTerms[ScalaLanguage, Target],
      ArrayProtocolTerms[ScalaLanguage, Target],
      PolyProtocolTerms[ScalaLanguage, Target]
  ) = (
    CirceProtocolGenerator.ProtocolSupportTermInterp,
    new CirceProtocolGenerator.ModelProtocolTermInterp(circeVersion),
    CirceProtocolGenerator.EnumProtocolTermInterp,
    CirceProtocolGenerator.ArrayProtocolTermInterp,
    CirceProtocolGenerator.PolyProtocolTermInterp
  )

  def circeJava8(circeVersion: CirceVersion): (
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
    new CirceProtocolGenerator.ModelProtocolTermInterp(circeVersion),
    CirceProtocolGenerator.EnumProtocolTermInterp,
    CirceProtocolGenerator.ArrayProtocolTermInterp,
    CirceProtocolGenerator.PolyProtocolTermInterp
  )

  def akkaHttp(circeVersion: CirceVersion): (
      ClientTerms[ScalaLanguage, Target],
      ServerTerms[ScalaLanguage, Target],
      FrameworkTerms[ScalaLanguage, Target]
  ) = (AkkaHttpClientGenerator.ClientTermInterp, AkkaHttpServerGenerator.ServerTermInterp, new AkkaHttpGenerator.FrameworkInterp(circeVersion))

  def endpoints(circeVersion: CirceVersion): (
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

  def extract(modules: NonEmptyList[String]): Target[Framework[ScalaLanguage, Target]] =
    (for {
      (circeVersion, (protocol, model, enum, array, poly)) <- popModule(
        "json",
        ("circe-java8", (CirceVersion.V011, circeJava8(CirceVersion.V011))),
        ("circe-0.11", (CirceVersion.V011, circe(CirceVersion.V011))),
        ("circe", (CirceVersion.V012, circe(CirceVersion.V012)))
      )
      (client, server, framework) <- popModule(
        "framework",
        ("akka-http", akkaHttp(circeVersion)),
        ("http4s", http4s),
        ("endpoints", endpoints(circeVersion))
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
