package com.twilio.swagger.codegen
package terms.protocol

import _root_.io.swagger.models.ArrayModel
import cats.free.{Free, Inject}
import scala.meta._

class ArrayProtocolTerms[F[_]](implicit I: Inject[ArrayProtocolTerm, F]) {
  def extractArrayType(arr: ArrayModel): Free[F, Type] =
    Free.inject[ArrayProtocolTerm, F](ExtractArrayType(arr))
}

object ArrayProtocolTerms {
  implicit def arrayProtocolTerms[F[_]](implicit I: Inject[ArrayProtocolTerm, F]): ArrayProtocolTerms[F] = new ArrayProtocolTerms[F]
}
