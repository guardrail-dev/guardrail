package com.twilio.swagger.codegen

import scala.meta._

sealed trait ProtocolElems

sealed trait StrictProtocolElems extends ProtocolElems
case class RandomType(tpe: Type, defn: List[Defn]) extends StrictProtocolElems
case class ClassDefinition(tpe: Type.Name, cls: Defn.Class, companion: Defn.Object) extends StrictProtocolElems
case class EnumDefinition(tpe: Type.Name, elems: List[(String, Term.Name, Term.Select)], cls: Defn.Class, companion: Defn.Object) extends StrictProtocolElems
