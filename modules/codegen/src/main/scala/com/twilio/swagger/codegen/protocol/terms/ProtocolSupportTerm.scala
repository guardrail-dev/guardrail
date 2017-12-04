package com.twilio.swagger.codegen
package terms.protocol

import scala.meta._
import _root_.io.swagger.models.Model

case class PropMeta(clsName: String, tpe: Type)
sealed trait ProtocolSupportTerm[T]
case class ExtractConcreteTypes(models: List[(String, Model)]) extends ProtocolSupportTerm[List[PropMeta]]
case class ProtocolImports() extends ProtocolSupportTerm[List[Import]]
case class PackageObjectImports() extends ProtocolSupportTerm[List[Import]]
case class PackageObjectContents() extends ProtocolSupportTerm[List[Stat]]
