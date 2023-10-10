package dev.guardrail.terms

import cats.Monad
import cats.data.NonEmptyList
import _root_.io.swagger.v3.oas.models.OpenAPI
import dev.guardrail.core.{ SupportDefinition, Tracker }
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.PropertyRequirement
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.terms.framework.FrameworkTerms

import scala.collection.immutable.List

abstract class ProtocolTerms[L <: LA, F[_]] { self =>
  def MonadF: Monad[F]

  def staticProtocolImports(pkgName: List[String]): F[List[L#Import]]
  def generateSupportDefinitions(): F[List[SupportDefinition[L]]]

  def fromSwagger(
      swagger: Tracker[OpenAPI],
      dtoPackage: List[String],
      supportPackage: NonEmptyList[String],
      defaultPropertyRequirement: PropertyRequirement
  )(implicit
      F: FrameworkTerms[L, F],
      P: ProtocolTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[ProtocolDefinitions[L]]

  def buildAccessor(clsName: String, termName: String): F[L#TermSelect]
}
