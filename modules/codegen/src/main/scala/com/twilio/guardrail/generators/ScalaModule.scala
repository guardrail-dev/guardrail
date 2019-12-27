package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.ScalaLanguage
import cats.data.NonEmptyList
import cats.arrow.FunctionK

object ScalaModule extends AbstractModule[ScalaLanguage] {
  implicit val coreTargetMonad: cats.Monad[CoreTarget] = cats.data.EitherT.catsDataMonadErrorForEitherT[cats.Id, Error]

  def circe: FunctionK[ModelInterpreters[ScalaLanguage, ?], Target] = {
    val interpDefinitionPM
        : FunctionK[DefinitionPM[ScalaLanguage, ?], Target]                         = CirceProtocolGenerator.ProtocolSupportTermInterp or CirceProtocolGenerator.ModelProtocolTermInterp
    val interpDefinitionPME: FunctionK[DefinitionPME[ScalaLanguage, ?], Target]     = CirceProtocolGenerator.EnumProtocolTermInterp or interpDefinitionPM
    val interpDefinitionPMEA: FunctionK[DefinitionPMEA[ScalaLanguage, ?], Target]   = CirceProtocolGenerator.ArrayProtocolTermInterp or interpDefinitionPME
    val interpDefinitionPMEAP: FunctionK[DefinitionPMEAP[ScalaLanguage, ?], Target] = CirceProtocolGenerator.PolyProtocolTermInterp or interpDefinitionPMEA
    interpDefinitionPMEAP
  }

  def circeJava8: FunctionK[ModelInterpreters[ScalaLanguage, ?], Target] = {
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

    val interpDefinitionPM: FunctionK[DefinitionPM[ScalaLanguage, ?], Target]       = java8timeCirceInterp or CirceProtocolGenerator.ModelProtocolTermInterp
    val interpDefinitionPME: FunctionK[DefinitionPME[ScalaLanguage, ?], Target]     = CirceProtocolGenerator.EnumProtocolTermInterp or interpDefinitionPM
    val interpDefinitionPMEA: FunctionK[DefinitionPMEA[ScalaLanguage, ?], Target]   = CirceProtocolGenerator.ArrayProtocolTermInterp or interpDefinitionPME
    val interpDefinitionPMEAP: FunctionK[DefinitionPMEAP[ScalaLanguage, ?], Target] = CirceProtocolGenerator.PolyProtocolTermInterp or interpDefinitionPMEA
    interpDefinitionPMEAP
  }

  def akkaHttp(interpModel: FunctionK[ModelInterpreters[ScalaLanguage, ?], Target]): FunctionK[ClientServerTerms[ScalaLanguage, ?], Target] = {
    val interpFrameworkC: FunctionK[FrameworkC[ScalaLanguage, ?], Target]     = AkkaHttpClientGenerator.ClientTermInterp or interpModel
    val interpFrameworkCS: FunctionK[FrameworkCS[ScalaLanguage, ?], Target]   = AkkaHttpServerGenerator.ServerTermInterp or interpFrameworkC
    val interpFrameworkCSF: FunctionK[FrameworkCSF[ScalaLanguage, ?], Target] = AkkaHttpGenerator.FrameworkInterp or interpFrameworkCS
    interpFrameworkCSF
  }

  def endpoints(interpModel: FunctionK[ModelInterpreters[ScalaLanguage, ?], Target]): FunctionK[ClientServerTerms[ScalaLanguage, ?], Target] = {
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

  def extract(modules: NonEmptyList[String]): CoreTarget[FunctionK[CodegenApplication[ScalaLanguage, ?], Target]] =
    (for {
      protocolGenerator <- popModule("json", ("circe-java8", circeJava8), ("circe", circe))
      interpFramework <- popModule(
        "framework",
        ("akka-http", akkaHttp(protocolGenerator)),
        ("http4s", http4s(protocolGenerator)),
        ("endpoints", endpoints(protocolGenerator))
      )
      parser             = SwaggerGenerator[ScalaLanguage] or interpFramework
      codegenApplication = ScalaGenerator.ScalaInterp or parser
    } yield codegenApplication).runA(modules.toList.toSet)
}
