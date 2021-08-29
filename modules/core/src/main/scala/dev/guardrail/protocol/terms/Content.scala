package dev.guardrail.protocol.terms

import cats.Order
import cats.syntax.all._

import java.util.Locale

sealed abstract class ContentType(val value: String) {
  override val toString: String = value
}
sealed class TextContent(value: String)   extends ContentType(value)
sealed class BinaryContent(value: String) extends ContentType(value)

object TextContent {
  def unapply(contentType: ContentType): Option[String] = contentType match {
    case tc: TextContent => Some(tc.value)
    case _               => None
  }
}

object BinaryContent {
  def unapply(contentType: ContentType): Option[String] = contentType match {
    case bc: BinaryContent => Some(bc.value)
    case _                 => None
  }
}

case object ApplicationJson    extends TextContent("application/json")
case object MultipartFormData  extends TextContent("multipart/form-data")
case object UrlencodedFormData extends TextContent("application/x-www-form-urlencoded")
case object TextPlain          extends TextContent("text/plain")
case object OctetStream        extends BinaryContent("application/octet-stream")
case object AnyContentType     extends ContentType("*/*")

object ContentType {
  // This is not intended to be exhaustive, but should hopefully cover cases we're likely to see.
  // See https://www.iana.org/assignments/media-types/media-types.xhtml for all IANA registered types.
  // Note that types that end in "+json" or "+xml" are handled explicitly in the match statement.
  private val APPLICATION_TEXT_TYPES = Set(
    "application/jose",
    "application/jwt",
    "application/mbox",
    "application/node",
    "application/sdp",
    "application/sgml",
    "application/sql",
    "application/x-pem-file",
    "application/xml",
    "application/xml-dtd",
    "application/xml-external-parsed-entity"
  )

  private def isApplicationText(name: String) =
    name.startsWith("application/") && (name.endsWith("+json") || name.endsWith("+xml") || APPLICATION_TEXT_TYPES.contains(name))

  private def isTextRegistry(name: String) = name.startsWith("text/")

  private def isUnsupported(name: String) =
    name == "application/xml" || // We explicitly avoid support for XML for now to avoid breaking existing specs
      (name.startsWith("multipart/") && name != "multipart/form-data") ||
      name.startsWith("example/")

  def unapply(value: String): Option[ContentType] = value.toLowerCase(Locale.US) match {
    case "application/json"                            => Some(ApplicationJson)
    case "multipart/form-data"                         => Some(MultipartFormData)
    case "application/x-www-form-urlencoded"           => Some(UrlencodedFormData)
    case "text/plain"                                  => Some(TextPlain)
    case "application/octet-stream"                    => Some(OctetStream)
    case "*/*"                                         => Some(AnyContentType)
    case x if isUnsupported(x)                         => None
    case x if isTextRegistry(x) | isApplicationText(x) => Some(new TextContent(value))
    case _                                             => Some(new BinaryContent(value))
  }

  implicit val ContentTypeOrder: Order[ContentType] = Order[String].contramap[ContentType](_.value)
}
