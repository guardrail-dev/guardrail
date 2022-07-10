package dev.guardrail.generators.scala.akkaHttp

import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail.{ RuntimeFailure, Target }
import dev.guardrail.core.Tracker
import dev.guardrail.generators.scala.{ CirceModelGenerator, JacksonModelGenerator, ModelGeneratorType }
import dev.guardrail.terms.{ ApplicationJson, ContentType, TextPlain }
import scala.meta._

object AkkaHttpHelper {
  def generateDecoder(tpe: Type, consumes: Tracker[NonEmptyList[ContentType]], modelGeneratorType: ModelGeneratorType): Target[(Term, Type)] = {
    val baseType = tpe match {
      case t"Option[$x]" => x
      case x             => x
    }

    for {
      unmarshallers <- consumes.indexedDistribute.toList.flatTraverse {
        case Tracker(_, _: ApplicationJson) => Target.pure(List(q"structuredJsonEntityUnmarshaller"))
        case Tracker(_, _: TextPlain)       => Target.pure(List(q"stringyJsonEntityUnmarshaller"))
        case Tracker(history, contentType)  => Target.log.warning(s"Unable to generate decoder for ${contentType} (${history})").map(_ => List.empty[Term.Name])
      }
      unmarshaller <- unmarshallers match {
        case Nil      => Target.raiseUserError(s"No decoders available (${consumes.showHistory})")
        case x :: Nil => Target.pure(x)
        case xs       => Target.pure(q"Unmarshaller.firstOf(..${xs})")
      }
      decode <- Target.fromOption(
        modelGeneratorType match {
          case _: CirceModelGenerator => Some(q"""io.circe.Decoder[${baseType}].decodeJson(json).fold(FastFuture.failed, FastFuture.successful)""")
          case _: JacksonModelGenerator =>
            Some(q"""FastFuture(implicitly[GuardrailDecoder[$baseType]].decode(json))""")
          case _ => None
        },
        RuntimeFailure(s"Unknown modelGeneratorType: ${modelGeneratorType}")
      )
    } yield {
      val decoder = q"""{ ${unmarshaller}.flatMap(_ => _ => json => $decode) }"""
      (decoder, baseType)
    }
  }

  def fromStringConverter(tpe: Type, modelGeneratorType: ModelGeneratorType): Either[String, Term] = modelGeneratorType match {
    case _: CirceModelGenerator   => Right(q"_root_.io.circe.Json.fromString(str).as[$tpe].toOption")
    case _: JacksonModelGenerator => Right(q"scala.util.Try(mapper.convertValue(str, new com.fasterxml.jackson.core.`type`.TypeReference[$tpe] {})).toOption")
    case _                        => Left(s"Unknown modelGeneratorType: ${modelGeneratorType}")
  }

  def protocolImplicits(modelGeneratorType: ModelGeneratorType): Target[List[Term.Param]] = modelGeneratorType match {
    case _: CirceModelGenerator => Target.pure(List.empty)
    case _: JacksonModelGenerator =>
      Target.pure(
        List(
          param"implicit mapper: com.fasterxml.jackson.databind.ObjectMapper",
          param"implicit validator: javax.validation.Validator"
        )
      )
    case _ => Target.raiseError(RuntimeFailure(s"Unknown modelGeneratorType: ${modelGeneratorType}"))
  }

  def unmarshalFieldTypeParam(modelGeneratorType: ModelGeneratorType): Target[Type.Param] = modelGeneratorType match {
    case _: CirceModelGenerator   => Target.pure(tparam"A: io.circe.Decoder")
    case _: JacksonModelGenerator => Target.pure(tparam"A: GuardrailDecoder: GuardrailValidator: scala.reflect.ClassTag")
    case _                        => Target.raiseError(RuntimeFailure(s"Unknown modelGeneratorType: ${modelGeneratorType}"))
  }

  def unmarshalFieldUnmarshallerType(modelGeneratorType: ModelGeneratorType): Target[Type] = modelGeneratorType match {
    case _: CirceModelGenerator   => Target.pure(t"io.circe.Json")
    case _: JacksonModelGenerator => Target.pure(t"com.fasterxml.jackson.databind.JsonNode")
    case _                        => Target.raiseError(RuntimeFailure(s"Unknown modelGeneratorType: ${modelGeneratorType}"))
  }
}
