package com.twilio.guardrail

package object extract {
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

  def TracingLabel[F: VendorExtension.VendorExtensible](v: F): Option[String] =
    extractWithFallback[F, String](v, "x-tracing-label", "x-scala-tracing-label")

  def PackageName[F: VendorExtension.VendorExtensible](v: F, vendorPrefixes: List[String]): Option[String] =
    extractFromNames[String, F](vendorPrefixes.map(_ + "-package"), v)

  def ServerRawResponse[F: VendorExtension.VendorExtensible](v: F): Option[Boolean] =
    VendorExtension(v).extract[Boolean]("x-server-raw-response")

  def EmptyValueIsNull[F: VendorExtension.VendorExtensible](v: F): Option[EmptyToNullBehaviour] =
    extractWithFallback[F, EmptyToNullBehaviour](v, "x-empty-is-null", "x-scala-empty-is-null")

  def FileHashAlgorithm[F: VendorExtension.VendorExtensible](v: F): Option[String] =
    extractWithFallback[F, String](v, "x-file-hash", "x-scala-file-hash")

  def extractFromNames[T: Extractable, F: VendorExtension.VendorExtensible](names: List[String], v: F): Option[T] =
    names.foldLeft(Option.empty[T])(
      (accum, name) => accum.orElse(VendorExtension(v).extract[T](name))
    )
}
