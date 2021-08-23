package dev.guardrail.generators.Scala

import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail.Target
import dev.guardrail.core.Tracker
import dev.guardrail.generators.Scala.model.{ CirceModelGenerator, JacksonModelGenerator, ModelGeneratorType }
import dev.guardrail.protocol.terms.{ ApplicationJson, ContentType, TextPlain }
import scala.meta._

object AkkaHttpHelper {
  def generateDecoder(tpe: Type, consumes: Tracker[NonEmptyList[ContentType]], modelGeneratorType: ModelGeneratorType): Target[(Term, Type)] = {
    val baseType = tpe match {
      case t"Option[$x]" => x
      case x             => x
    }

    for {
      unmarshallers <- consumes.indexedDistribute.toList.flatTraverse {
        case Tracker(_, ApplicationJson)   => Target.pure(List(q"structuredJsonEntityUnmarshaller"))
        case Tracker(_, TextPlain)         => Target.pure(List(q"stringyJsonEntityUnmarshaller"))
        case Tracker(history, contentType) => Target.log.warning(s"Unable to generate decoder for ${contentType} (${history})").map(_ => List.empty[Term.Name])
      }
      unmarshaller <- unmarshallers match {
        case Nil      => Target.raiseUserError(s"No decoders available (${consumes.showHistory})")
        case x :: Nil => Target.pure(x)
        case xs       => Target.pure(q"Unmarshaller.firstOf(..${xs})")
      }
    } yield {
      val decode = modelGeneratorType match {
        case _: CirceModelGenerator => q"""io.circe.Decoder[${baseType}].decodeJson(json).fold(FastFuture.failed, FastFuture.successful)"""
        case JacksonModelGenerator =>
          q"""FastFuture(implicitly[GuardrailDecoder[$baseType]].decode(json))"""
      }
      val decoder = q"""{ ${unmarshaller}.flatMap(_ => _ => json => $decode) }"""
      (decoder, baseType)
    }
  }

  def fromStringConverter(tpe: Type, modelGeneratorType: ModelGeneratorType): Term = modelGeneratorType match {
    case _: CirceModelGenerator => q"io.circe.Json.fromString(str).as[$tpe].toOption"
    case JacksonModelGenerator  => q"scala.util.Try(mapper.convertValue(str, new com.fasterxml.jackson.core.`type`.TypeReference[$tpe] {})).toOption"
  }

  def protocolImplicits(modelGeneratorType: ModelGeneratorType): List[Term.Param] = modelGeneratorType match {
    case _: CirceModelGenerator => List.empty
    case JacksonModelGenerator =>
      List(param"implicit mapper: com.fasterxml.jackson.databind.ObjectMapper", param"implicit validator: javax.validation.Validator")
  }

  def unmarshalFieldTypeParam(modelGeneratorType: ModelGeneratorType): Type.Param = modelGeneratorType match {
    case _: CirceModelGenerator => tparam"A: io.circe.Decoder"
    case JacksonModelGenerator  => tparam"A: GuardrailDecoder: GuardrailValidator: scala.reflect.ClassTag"
  }

  def unmarshalFieldUnmarshallerType(modelGeneratorType: ModelGeneratorType): Type = modelGeneratorType match {
    case _: CirceModelGenerator => t"io.circe.Json"
    case JacksonModelGenerator  => t"com.fasterxml.jackson.databind.JsonNode"
  }
}
