package com.twilio.swagger.codegen
package terms.protocol

import scala.meta._

sealed trait ProtocolSupportTerm[T]
case class ProtocolImports() extends ProtocolSupportTerm[List[Import]]
case class PackageObjectImports() extends ProtocolSupportTerm[List[Import]]
case class PackageObjectContents() extends ProtocolSupportTerm[List[Stat]]
