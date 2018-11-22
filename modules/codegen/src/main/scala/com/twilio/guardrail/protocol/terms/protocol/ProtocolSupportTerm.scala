package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.Model
import com.twilio.guardrail.languages.{ LA, ScalaLanguage }
import com.twilio.guardrail.{ ProtocolElems, StrictProtocolElems }

case class PropMeta(clsName: String, tpe: ScalaLanguage#Type)
sealed trait ProtocolSupportTerm[L <: LA, T]
case class ExtractConcreteTypes[L <: LA](models: List[(String, Model)]) extends ProtocolSupportTerm[L, List[PropMeta]]
case class ProtocolImports[L <: LA]()                                   extends ProtocolSupportTerm[L, List[L#Import]]
case class PackageObjectImports[L <: LA]()                              extends ProtocolSupportTerm[L, List[L#Import]]
case class PackageObjectContents[L <: LA]()                             extends ProtocolSupportTerm[L, List[L#ValueDefinition]]
