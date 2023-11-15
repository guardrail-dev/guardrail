package dev.guardrail.terms.protocol

import dev.guardrail.languages.LA
import dev.guardrail.terms.RenderedEnum

case class StaticDefns[L <: LA](className: String, extraImports: List[L#Import], definitions: List[L#Definition])

sealed trait StrictProtocolElems[L <: LA] { def name: String }
case class RandomType[L <: LA](name: String, tpe: L#Type) extends StrictProtocolElems[L]

sealed trait NestedProtocolElems[L <: LA] extends StrictProtocolElems[L]

case class ClassDefinition[L <: LA](
    name: String,
    tpe: L#TypeName,
    fullType: L#Type,
    cls: L#ClassDefinition,
    staticDefns: StaticDefns[L],
    parents: List[SuperClass[L]] = Nil
) extends NestedProtocolElems[L]

case class ADT[L <: LA](name: String, tpe: L#TypeName, fullType: L#Type, trt: L#Trait, staticDefns: StaticDefns[L]) extends StrictProtocolElems[L]

case class EnumDefinition[L <: LA](
    name: String,
    tpe: L#TypeName,
    fullType: L#Type,
    elems: RenderedEnum[L],
    cls: L#ClassDefinition,
    staticDefns: StaticDefns[L]
) extends NestedProtocolElems[L]
