package com.twilio.swagger.codegen

import scala.meta._

sealed trait ProtocolElems

sealed trait StrictProtocolElems extends ProtocolElems { def name: String }
case class RandomType(name: String, tpe: Type, defn: List[Defn]) extends StrictProtocolElems
case class ClassDefinition(name: String, tpe: Type.Name, cls: Defn.Class, companion: Defn.Object) extends StrictProtocolElems
case class EnumDefinition(name: String, tpe: Type.Name, elems: List[(String, Term.Name, Term.Select)], cls: Defn.Class, companion: Defn.Object) extends StrictProtocolElems
