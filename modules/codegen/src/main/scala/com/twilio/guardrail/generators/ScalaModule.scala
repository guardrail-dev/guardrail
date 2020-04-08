package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.ScalaLanguage
import cats.data.NonEmptyList
import cats.arrow.FunctionK
import cats.implicits._
import com.twilio.guardrail.circe.CirceVersion

object ScalaModule extends AbstractModule[ScalaLanguage] {
  def circe(circeVersion: CirceVersion): FunctionK[ModelInterpreters[ScalaLanguage, ?], Target] = {
    val interpDefinitionPM
        : FunctionK[DefinitionPM[ScalaLanguage, ?], Target] = CirceProtocolGenerator.ProtocolSupportTermInterp or new CirceProtocolGenerator.ModelProtocolTermInterp(
            circeVersion
          )
    val interpDefinitionPME: FunctionK[DefinitionPME[ScalaLanguage, ?], Target]     = CirceProtocolGenerator.EnumProtocolTermInterp or interpDefinitionPM
    val interpDefinitionPMEA: FunctionK[DefinitionPMEA[ScalaLanguage, ?], Target]   = CirceProtocolGenerator.ArrayProtocolTermInterp or interpDefinitionPME
    val interpDefinitionPMEAP: FunctionK[DefinitionPMEAP[ScalaLanguage, ?], Target] = CirceProtocolGenerator.PolyProtocolTermInterp or interpDefinitionPMEA
    interpDefinitionPMEAP
  }

  def circeJava8(circeVersion: CirceVersion): FunctionK[ModelInterpreters[ScalaLanguage, ?], Target] = {
    import com.twilio.guardrail.protocol.terms.protocol.{ PackageObjectImports, ProtocolSupportTerm }
    val java8timeCirceInterp = new FunctionK[ProtocolSupportTerm[ScalaLanguage, ?], Target] {
      import scala.meta._
      def apply[A](value: ProtocolSupportTerm[ScalaLanguage, A]): Target[A] = value match {
        case PackageObjectImports() =>
          CirceProtocolGenerator.ProtocolSupportTermInterp(value).map { values =>
            values :+ q"import io.circe.java8.time._"
          }
        case other => CirceProtocolGenerator.ProtocolSupportTermInterp(other)
      }
    }

    val interpDefinitionPM: FunctionK[DefinitionPM[ScalaLanguage, ?], Target] = java8timeCirceInterp or new CirceProtocolGenerator.ModelProtocolTermInterp(
            circeVersion
          )
    val interpDefinitionPME: FunctionK[DefinitionPME[ScalaLanguage, ?], Target]     = CirceProtocolGenerator.EnumProtocolTermInterp or interpDefinitionPM
    val interpDefinitionPMEA: FunctionK[DefinitionPMEA[ScalaLanguage, ?], Target]   = CirceProtocolGenerator.ArrayProtocolTermInterp or interpDefinitionPME
    val interpDefinitionPMEAP: FunctionK[DefinitionPMEAP[ScalaLanguage, ?], Target] = CirceProtocolGenerator.PolyProtocolTermInterp or interpDefinitionPMEA
    interpDefinitionPMEAP
  }

  def akkaHttp(
      circeVersion: CirceVersion,
      interpModel: FunctionK[ModelInterpreters[ScalaLanguage, ?], Target]
  ): FunctionK[ClientServerTerms[ScalaLanguage, ?], Target] = {
    val interpFrameworkC: FunctionK[FrameworkC[ScalaLanguage, ?], Target]     = AkkaHttpClientGenerator.ClientTermInterp or interpModel
    val interpFrameworkCS: FunctionK[FrameworkCS[ScalaLanguage, ?], Target]   = AkkaHttpServerGenerator.ServerTermInterp or interpFrameworkC
    val interpFrameworkCSF: FunctionK[FrameworkCSF[ScalaLanguage, ?], Target] = new AkkaHttpGenerator.FrameworkInterp(circeVersion) or interpFrameworkCS
    interpFrameworkCSF
  }

  def endpoints(
      circeVersion: CirceVersion,
      interpModel: FunctionK[ModelInterpreters[ScalaLanguage, ?], Target]
  ): FunctionK[ClientServerTerms[ScalaLanguage, ?], Target] = {
    val interpFrameworkC: FunctionK[FrameworkC[ScalaLanguage, ?], Target]     = EndpointsClientGenerator.ClientTermInterp or interpModel
    val interpFrameworkCS: FunctionK[FrameworkCS[ScalaLanguage, ?], Target]   = EndpointsServerGenerator.ServerTermInterp or interpFrameworkC
    val interpFrameworkCSF: FunctionK[FrameworkCSF[ScalaLanguage, ?], Target] = EndpointsGenerator.FrameworkInterp or interpFrameworkCS
    interpFrameworkCSF
  }

  def http4s(interpModel: FunctionK[ModelInterpreters[ScalaLanguage, ?], Target]): FunctionK[ClientServerTerms[ScalaLanguage, ?], Target] = {
    val interpFrameworkC: FunctionK[FrameworkC[ScalaLanguage, ?], Target]     = Http4sClientGenerator.ClientTermInterp or interpModel
    val interpFrameworkCS: FunctionK[FrameworkCS[ScalaLanguage, ?], Target]   = Http4sServerGenerator.ServerTermInterp or interpFrameworkC
    val interpFrameworkCSF: FunctionK[FrameworkCSF[ScalaLanguage, ?], Target] = Http4sGenerator.FrameworkInterp or interpFrameworkCS
    interpFrameworkCSF
  }

  def extract(modules: NonEmptyList[String]): Target[FunctionK[CodegenApplication[ScalaLanguage, ?], Target]] =
    (for {
      (circeVersion, protocolGenerator) <- popModule(
        "json",
        ("circe-java8", (CirceVersion.V011, circeJava8(CirceVersion.V011))),
        ("circe-0.11", (CirceVersion.V011, circe(CirceVersion.V011))),
        ("circe", (CirceVersion.V012, circe(CirceVersion.V012)))
      )
      interpFramework <- popModule(
        "framework",
        ("akka-http", akkaHttp(circeVersion, protocolGenerator)),
        ("http4s", http4s(protocolGenerator)),
        ("endpoints", endpoints(circeVersion, protocolGenerator))
      )
      parser             = SwaggerGenerator[ScalaLanguage] or interpFramework
      codegenApplication = ScalaGenerator.ScalaInterp or parser
    } yield codegenApplication).runA(modules.toList.toSet)
}
