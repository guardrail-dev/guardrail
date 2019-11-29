package com.twilio.guardrail
package generators

import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.generators.Java.JacksonGenerator
import com.twilio.guardrail.generators.Java.AsyncHttpClientClientGenerator
import com.twilio.guardrail.generators.Java.DropwizardGenerator.FrameworkInterp
import com.twilio.guardrail.generators.Java.DropwizardServerGenerator
import com.twilio.guardrail.protocol.terms.client.ClientTerm
import com.twilio.guardrail.protocol.terms.server.ServerTerm
import cats.arrow.FunctionK
import cats.data.NonEmptyList

object JavaModule extends AbstractModule[JavaLanguage] {
  def jackson: FunctionK[ModelInterpreters[JavaLanguage, ?], Target] = {
    val interpDefinitionPM
        : FunctionK[DefinitionPM[JavaLanguage, ?], Target]                         = JacksonGenerator.ProtocolSupportTermInterp or JacksonGenerator.ModelProtocolTermInterp
    val interpDefinitionPME: FunctionK[DefinitionPME[JavaLanguage, ?], Target]     = JacksonGenerator.EnumProtocolTermInterp or interpDefinitionPM
    val interpDefinitionPMEA: FunctionK[DefinitionPMEA[JavaLanguage, ?], Target]   = JacksonGenerator.ArrayProtocolTermInterp or interpDefinitionPME
    val interpDefinitionPMEAP: FunctionK[DefinitionPMEAP[JavaLanguage, ?], Target] = JacksonGenerator.PolyProtocolTermInterp or interpDefinitionPMEA
    interpDefinitionPMEAP
  }

  def dropwizard: FunctionK[ServerTerm[JavaLanguage, ?], Target]      = DropwizardServerGenerator.ServerTermInterp
  def asyncHttpClient: FunctionK[ClientTerm[JavaLanguage, ?], Target] = AsyncHttpClientClientGenerator.ClientTermInterp

  def extract(modules: NonEmptyList[String]): CoreTarget[FunctionK[CodegenApplication[JavaLanguage, ?], Target]] =
    (for {
      protocolGenerator <- popModule("json", ("jackson", jackson))
      clientGenerator   <- popModule("client", ("async-http-client", asyncHttpClient))
      serverGenerator   <- popModule("server", ("dropwizard", dropwizard))
      interpFrameworkC: FunctionK[FrameworkC[JavaLanguage, ?], Target]     = clientGenerator or protocolGenerator
      interpFrameworkCS: FunctionK[FrameworkCS[JavaLanguage, ?], Target]   = serverGenerator or interpFrameworkC
      interpFrameworkCSF: FunctionK[FrameworkCSF[JavaLanguage, ?], Target] = FrameworkInterp or interpFrameworkCS

      parser             = SwaggerGenerator[JavaLanguage] or interpFrameworkCSF
      codegenApplication = JavaGenerator.JavaInterp or parser
    } yield codegenApplication).runA(modules.toList.toSet)
}
