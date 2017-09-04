package com.twilio.swagger.codegen
package terms.protocol

import scala.collection.immutable.Seq
import scala.meta._

sealed trait ProtocolSupportTerm[T]
case class ProtocolImports() extends ProtocolSupportTerm[Seq[Import]]
case class PackageObjectImports() extends ProtocolSupportTerm[Seq[Import]]
case class PackageObjectContents() extends ProtocolSupportTerm[Seq[Stat]]
