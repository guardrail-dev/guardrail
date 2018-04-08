package com.twilio.guardrail
package terms.protocol

import _root_.io.swagger.models.ArrayModel
import scala.meta._

sealed trait ArrayProtocolTerm[T]
case class ExtractArrayType(arr: ArrayModel, concreteTypes: List[PropMeta]) extends ArrayProtocolTerm[Type]
