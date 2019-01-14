package com.twilio.guardrail.generators

import com.twilio.guardrail.terms.RouteMeta
import scala.meta._

object AkkaHttpHelper {
  def generateDecoder(tpe: Type, consumes: Seq[RouteMeta.ContentType]): (Term, Type) = {
    val baseType = tpe match {
      case t"Option[$x]" => x
      case x             => x
    }
    val jsonUnmarshaller =
      if (consumes.contains(RouteMeta.ApplicationJson) || consumes.isEmpty) { List(q"structuredJsonEntityUnmarshaller") } else { List.empty }
    val textUnmarshaller = if (consumes.contains(RouteMeta.TextPlain)) { List(q"stringyJsonEntityUnmarshaller") } else { List.empty }

    val unmarshaller = (jsonUnmarshaller ++ textUnmarshaller) match {
      case x :: Nil => x
      case xs       => q"Unmarshaller.firstOf(..${xs})"
    }
    val decoder = q""" {
      ${unmarshaller}.flatMap(_ => _ => json => io.circe.Decoder[${baseType}].decodeJson(json).fold(FastFuture.failed, FastFuture.successful))
    }
    """
    (decoder, baseType)
  }
}
