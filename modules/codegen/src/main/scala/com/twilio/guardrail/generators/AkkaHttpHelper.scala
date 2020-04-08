package com.twilio.guardrail.generators

import cats.data.NonEmptyList
import cats.implicits._
import com.twilio.guardrail.Target
import com.twilio.guardrail.protocol.terms.{ ApplicationJson, ContentType, TextPlain }
import scala.meta._

object AkkaHttpHelper {
  def generateDecoder(tpe: Type, consumes: NonEmptyList[ContentType]): Target[(Term, Type)] = {
    val baseType = tpe match {
      case t"Option[$x]" => x
      case x             => x
    }

    for {
      unmarshallers <- consumes.toList.flatTraverse[Target, Term]({
        case ApplicationJson => Target.pure(List(q"structuredJsonEntityUnmarshaller"))
        case TextPlain       => Target.pure(List(q"stringyJsonEntityUnmarshaller"))
        case contentType     => Target.log.warning(s"Unable to generate decoder for ${contentType}").map(_ => List.empty)
      })
      unmarshaller <- unmarshallers match {
        case Nil      => Target.raiseUserError(s"No decoders available")
        case x :: Nil => Target.pure(x)
        case xs       => Target.pure(q"Unmarshaller.firstOf(..${xs})")
      }
    } yield {
      val decoder = q""" {
        ${unmarshaller}.flatMap(_ => _ => json => io.circe.Decoder[${baseType}].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
      }
      """
      (decoder, baseType)
    }
  }
}
