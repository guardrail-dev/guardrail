package com.twilio.guardrail.protocol.terms

import cats.Order
import cats.implicits._
import java.util.Locale

sealed abstract class ContentType(val value: String) {
  override val toString: String = value
}
case object ApplicationJson    extends ContentType("application/json")
case object MultipartFormData  extends ContentType("multipart/form-data")
case object UrlencodedFormData extends ContentType("application/x-www-form-urlencoded")
case object TextPlain          extends ContentType("text/plain")
case object OctetStream        extends ContentType("application/octet-stream")

object ContentType {
  def unapply(value: String): Option[ContentType] = value.toLowerCase(Locale.US) match {
    case "application/json"                  => Some(ApplicationJson)
    case "multipart/form-data"               => Some(MultipartFormData)
    case "application/x-www-form-urlencoded" => Some(UrlencodedFormData)
    case "text/plain"                        => Some(TextPlain)
    case "application/octet-stream"          => Some(OctetStream)
    case _                                   => None
  }
  implicit val ContentTypeOrder: Order[ContentType] = Order[String].contramap[ContentType](_.value)
}
