package dev.guardrail.terms

import cats.Order
import cats.syntax.all._

import java.util.Locale
import scala.reflect.ClassTag

sealed abstract class ContentType(val value: String) {
  override val toString: String = value
  override def equals(x: Any): Boolean = x match {
    case x: ContentType => x.value == value
    case _              => false
  }
}
sealed class TextContent(value: String)   extends ContentType(value)
sealed class BinaryContent(value: String) extends ContentType(value)

class ApplicationJson private (val scope: Option[String]) extends TextContent(s"application/${scope.fold("")(_ + "+")}json") {
  def withScope(scope: Option[String]): ApplicationJson = new ApplicationJson(scope)
}
object ApplicationJson {
  val empty: ApplicationJson = new ApplicationJson(None)
}
class MultipartFormData private () extends TextContent("multipart/form-data")
object MultipartFormData {
  val empty: MultipartFormData = new MultipartFormData()
}
class UrlencodedFormData private () extends TextContent("application/x-www-form-urlencoded")
object UrlencodedFormData {
  val empty: UrlencodedFormData = new UrlencodedFormData()
}
class TextPlain private () extends TextContent("text/plain")
object TextPlain {
  val empty: TextPlain = new TextPlain()
}
class OctetStream private () extends BinaryContent("application/octet-stream")
object OctetStream {
  val empty: OctetStream = new OctetStream()
}
class AnyContentType private () extends ContentType("*/*")
object AnyContentType {
  val empty: AnyContentType = new AnyContentType()
}

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
    case "application/json"                            => Some(ApplicationJson.empty)
    case "multipart/form-data"                         => Some(MultipartFormData.empty)
    case "application/x-www-form-urlencoded"           => Some(UrlencodedFormData.empty)
    case "text/plain"                                  => Some(TextPlain.empty)
    case "application/octet-stream"                    => Some(OctetStream.empty)
    case "*/*"                                         => Some(AnyContentType.empty)
    case x if isUnsupported(x)                         => None
    case x if isTextRegistry(x) | isApplicationText(x) => Some(new TextContent(value))
    case _                                             => Some(new BinaryContent(value))
  }

  implicit val ContentTypeOrder: Order[ContentType] = Order[String].contramap[ContentType](_.value)

  def isSubtypeOf[A](a: ContentType)(implicit ev: ClassTag[A]): Boolean = a match {
    case a: A => true
    case _    => false
  }
}
