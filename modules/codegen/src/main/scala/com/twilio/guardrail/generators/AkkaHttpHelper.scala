package com.twilio.guardrail.generators

import cats.data.NonEmptyList
import com.twilio.guardrail.terms.RouteMeta
import scala.meta._

object AkkaHttpHelper {
  def generateDecoder(tpe: Type, consumes: NonEmptyList[RouteMeta.ContentType]): (Term, Type) = {
    val baseType = tpe match {
      case t"Option[$x]" => x
      case x             => x
    }

    val unmarshaller = consumes.map {
      case RouteMeta.ApplicationJson => q"structuredJsonEntityUnmarshaller"
      case RouteMeta.TextPlain       => q"stringyJsonEntityUnmarshaller"
      case contentType               => throw new Exception(s"Unable to generate decoder for ${contentType}")
    } match {
      case NonEmptyList(x, Nil) => x
      case xs                   => q"Unmarshaller.firstOf(..${xs.toList})"
    }
    val decoder = q""" {
      ${unmarshaller}.flatMap(_ => _ => json => io.circe.Decoder[${baseType}].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
    }
    """
    (decoder, baseType)
  }
}
