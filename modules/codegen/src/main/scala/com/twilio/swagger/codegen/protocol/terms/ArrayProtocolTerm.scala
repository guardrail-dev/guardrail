package com.twilio.swagger.codegen
package terms.protocol

import _root_.io.swagger.models.ArrayModel
import scala.meta._

sealed trait ArrayProtocolTerm[T]
case class ExtractArrayType(arr: ArrayModel) extends ArrayProtocolTerm[Type]
