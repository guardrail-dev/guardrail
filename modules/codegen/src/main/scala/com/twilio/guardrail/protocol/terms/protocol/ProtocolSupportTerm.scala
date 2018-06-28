package com.twilio.guardrail.protocol.terms.protocol

import _root_.io.swagger.models.Model

import scala.meta._

case class PropMeta(clsName: String, tpe: Type)
sealed trait ProtocolSupportTerm[T]
case class ExtractConcreteTypes(models: List[(String, Model)]) extends ProtocolSupportTerm[List[PropMeta]]
case class ProtocolImports()                                   extends ProtocolSupportTerm[List[Import]]
case class PackageObjectImports()                              extends ProtocolSupportTerm[List[Import]]
case class PackageObjectContents()                             extends ProtocolSupportTerm[List[Stat]]
