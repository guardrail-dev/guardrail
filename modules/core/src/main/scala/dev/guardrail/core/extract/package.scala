package dev.guardrail.core

import cats.syntax.foldable._

package object extract {
  private def extractFromNames[F: VendorExtension.VendorExtensible, T: Extractable](v: F, names: List[String]): Option[T] =
    names.collectFirstSome(VendorExtension(v).extract[T](_))

  private def extractName[F: VendorExtension.VendorExtensible, T: Extractable](v: F, name: String): Option[T] = {
    val ve = VendorExtension(v)
    ve.extract[T](name)
  }

  def CustomTypeName[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[F, String](v, vendorPrefixes.map(_ + "-type"))

  def CustomArrayTypeName[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[F, String](v, vendorPrefixes.map(_ + "-array-type"))

  def CustomMapTypeName[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[F, String](v, vendorPrefixes.map(_ + "-map-type"))

  def TracingLabel[F: VendorExtension.VendorExtensible](v: F): Option[String] =
    extractName[F, String](v, "x-tracing-label")

  def PackageName[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[F, String](v, vendorPrefixes.map(_ + "-package"))

  def ClassPrefix[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[F, String](v, vendorPrefixes.map(_ + "-class-prefix"))

  def ServerRawResponse[F: VendorExtension.VendorExtensible](v: F): Option[Boolean] =
    VendorExtension(v).extract[Boolean]("x-server-raw-response")

  def EmptyValueIsNull[F: VendorExtension.VendorExtensible](v: F): Option[EmptyToNullBehaviour] =
    extractName[F, EmptyToNullBehaviour](v, "x-empty-is-null")

  def FileHashAlgorithm[F: VendorExtension.VendorExtensible](v: F): Option[String] =
    extractName[F, String](v, "x-file-hash")

  def DataRedaction[F: VendorExtension.VendorExtensible](v: F): Option[RedactionBehaviour] =
    VendorExtension(v).extract[RedactionBehaviour]("x-data-redaction")
}
