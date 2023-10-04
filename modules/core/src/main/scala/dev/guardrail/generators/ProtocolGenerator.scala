package dev.guardrail.generators

import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, Schema }
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.StrictProtocolElems
import dev.guardrail.terms.{ EnumSchema, NumberEnumSchema, ObjectEnumSchema, StringEnumSchema }

case class ProtocolDefinitions[L <: LA](
    elems: List[StrictProtocolElems[L]],
    protocolImports: List[L#Import],
    packageObjectImports: List[L#Import],
    packageObjectContents: List[L#Statement],
    implicitsObject: Option[(L#TermName, L#ObjectDefinition)]
)

object ProtocolGenerator {
  type WrapEnumSchema[A] = Schema[A] => EnumSchema
  implicit val wrapNumberEnumSchema: WrapEnumSchema[Number] = NumberEnumSchema.apply _
  implicit val wrapObjectEnumSchema: WrapEnumSchema[Object] = ObjectEnumSchema.apply _
  implicit val wrapStringEnumSchema: WrapEnumSchema[String] = StringEnumSchema.apply _
}
