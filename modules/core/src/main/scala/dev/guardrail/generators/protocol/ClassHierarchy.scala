package dev.guardrail.generators.protocol

import _root_.io.swagger.v3.oas.models.media.Schema
import dev.guardrail.languages.LA
import dev.guardrail.core.Tracker
import dev.guardrail.terms.protocol.Discriminator

sealed trait ClassHierarchy[L <: LA] {
  def name: String
  def model: Tracker[Schema[_]]
  def children: List[ClassChild[L]]
  def required: List[String]
}
case class ClassChild[L <: LA](name: String, model: Tracker[Schema[_]], children: List[ClassChild[L]], required: List[String]) extends ClassHierarchy[L]
case class ClassParent[L <: LA](
    name: String,
    model: Tracker[Schema[_]],
    children: List[ClassChild[L]],
    discriminator: Discriminator[L],
    required: List[String]
) extends ClassHierarchy[L]
