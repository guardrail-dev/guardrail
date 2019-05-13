package com.twilio.guardrail

import cats.instances.list._
import cats.syntax.foldable._

package object extract {
  private def extractFromNames[F: VendorExtension.VendorExtensible, T: Extractable](v: F, names: List[String]): Option[T] =
    names.collectFirstSome(VendorExtension(v).extract[T](_))

  private def extractWithFallback[F: VendorExtension.VendorExtensible, T: Extractable](v: F, name: String, fallbackName: String): Option[T] = {
    val ve = VendorExtension(v)
    ve.extract[T](name)
      .orElse({
        val value = ve.extract[T](fallbackName)
        if (value.isDefined) {
          println(s"WARNING: Deprecated vendor extension '${fallbackName}'; please migrate to use '${name}' instead")
        }
        value
      })
  }

  def CustomTypeName[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[F, String](v, vendorPrefixes.map(_ + "-type"))

  def TracingLabel[F: VendorExtension.VendorExtensible](v: F): Option[String] =
    extractWithFallback[F, String](v, "x-tracing-label", "x-scala-tracing-label")

  def PackageName[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[F, String](v, vendorPrefixes.map(_ + "-package"))

  def ServerRawResponse[F: VendorExtension.VendorExtensible](v: F): Option[Boolean] =
    VendorExtension(v).extract[Boolean]("x-server-raw-response")

  def EmptyValueIsNull[F: VendorExtension.VendorExtensible](v: F): Option[EmptyToNullBehaviour] =
    extractWithFallback[F, EmptyToNullBehaviour](v, "x-empty-is-null", "x-scala-empty-is-null")

  def FileHashAlgorithm[F: VendorExtension.VendorExtensible](v: F): Option[String] =
    extractWithFallback[F, String](v, "x-file-hash", "x-scala-file-hash")

  def SecurityOptional[F: VendorExtension.VendorExtensible](v: F): List[String] =
    VendorExtension(v).extract[List[String]]("x-security-optional").toList.flatten
}
