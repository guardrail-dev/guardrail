package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.ArrayModel
import com.twilio.guardrail.generators.GeneratorSettings

import scala.meta._

sealed trait ArrayProtocolTerm[T]
case class ExtractArrayType(arr: ArrayModel, concreteTypes: List[PropMeta], generatorSettings: GeneratorSettings)
    extends ArrayProtocolTerm[Type]
