package com.twilio.guardrail.generators.Java

import cats.data.NonEmptyList
import cats.syntax.foldable._
import com.github.javaparser.ast.`type`.Type
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.generators.ScalaParameters
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms._
import io.swagger.v3.oas.models.Operation

object DropwizardHelpers {
  private val CONSUMES_PRIORITY = NonEmptyList.of(ApplicationJson, TextPlain, OctetStream)
  private val PRODUCES_PRIORITY = NonEmptyList.of(ApplicationJson, TextPlain, OctetStream)

  def getBestConsumes(operation: Tracker[Operation], contentTypes: List[ContentType], parameters: ScalaParameters[JavaLanguage]): Option[ContentType] =
    if (parameters.formParams.nonEmpty) {
      if (parameters.formParams.exists(_.isFile) || contentTypes.contains(MultipartFormData)) {
        Some(MultipartFormData)
      } else {
        Some(UrlencodedFormData)
      }
    } else {
      parameters.bodyParams.map({ bodyParam =>
        CONSUMES_PRIORITY
          .collectFirstSome(ct => contentTypes.find(_ == ct))
          .orElse(contentTypes.collectFirst({ case tc: TextContent => tc }))
          .orElse(contentTypes.collectFirst({ case bc: BinaryContent => bc }))
          .getOrElse({
            val fallback =
              if (bodyParam.argType.isPrimitiveType || bodyParam.argType.isNamed("String")) TextPlain
              else ApplicationJson
            println(s"WARNING: no supported body param type at ${operation.showHistory}; falling back to $fallback")
            fallback
          })
      })
    }

  def getBestProduces(operationId: String, contentTypes: List[ContentType], response: Response[JavaLanguage]): Option[ContentType] =
    response.value
      .map(_._1)
      .flatMap({ valueType: Type =>
        PRODUCES_PRIORITY
          .collectFirstSome(ct => contentTypes.find(_ == ct))
          .orElse(contentTypes.collectFirst({ case tc: TextContent => tc }))
          .orElse(contentTypes.collectFirst({ case bc: BinaryContent => bc }))
          .orElse({
            val fallback = if (valueType.isNamed("String")) TextPlain else ApplicationJson
            println(
              s"WARNING: no supported body param type for operation '$operationId', response code ${response.statusCode}; falling back to ${fallback.value}"
            )
            Option(fallback)
          })
      })
}
