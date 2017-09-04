package com.twilio.swagger.codegen
package terms.protocol

import scala.meta._

sealed trait AliasProtocolTerm[T]
case class RenderAlias(clsName: String, tpe: Type) extends AliasProtocolTerm[Defn]
case class RenderAliasCompanion(clsName: String) extends AliasProtocolTerm[Defn.Object]
