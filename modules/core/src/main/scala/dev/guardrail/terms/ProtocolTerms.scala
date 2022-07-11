package dev.guardrail.terms

import cats.Monad
import io.swagger.v3.oas.models.media.{ ComposedSchema, Schema }
import dev.guardrail.core.{ ResolvedType, SupportDefinition, Tracker }
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol.{ Discriminator, PropMeta, PropertyRequirement, ProtocolParameter, StaticDefns, SuperClass }

import scala.collection.immutable.List

abstract class ProtocolTerms[L <: LA, F[_]] { self =>
  def MonadF: Monad[F]

  // ProtocolTerms
  def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]): F[List[PropMeta[L]]]
  def staticProtocolImports(pkgName: List[String]): F[List[L#Import]]
  def protocolImports(): F[List[L#Import]]
  def packageObjectImports(): F[List[L#Import]]
  def packageObjectContents(): F[List[L#Statement]]
  def implicitsObject(): F[Option[(L#TermName, L#ObjectDefinition)]]
  def generateSupportDefinitions(): F[List[SupportDefinition[L]]]

  // ModelProtocolTerms
  def extractProperties(swagger: Tracker[Schema[_]]): F[List[(String, Tracker[Schema[_]])]]
  def transformProperty(clsName: String, dtoPackage: List[String], supportPackage: List[String], concreteTypes: List[PropMeta[L]])(
      name: String,
      fieldName: String,
      prop: Tracker[Schema[_]],
      meta: ResolvedType[L],
      requirement: PropertyRequirement,
      isCustomType: Boolean,
      defaultValue: Option[L#Term]
  ): F[ProtocolParameter[L]]
  def renderDTOClass(clsName: String, supportPackage: List[String], terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil): F[L#ClassDefinition]
  def encodeModel(
      clsName: String,
      dtoPackage: List[String],
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): F[Option[L#ValueDefinition]]
  def decodeModel(
      clsName: String,
      dtoPackage: List[String],
      supportPackage: List[String],
      params: List[ProtocolParameter[L]],
      parents: List[SuperClass[L]] = Nil
  ): F[Option[L#ValueDefinition]]
  def renderDTOStaticDefns(
      clsName: String,
      deps: List[L#TermName],
      encoder: Option[L#ValueDefinition],
      decoder: Option[L#ValueDefinition],
      protocolParameters: List[ProtocolParameter[L]]
  ): F[StaticDefns[L]]

  // ArrayProtocolTerms
  def extractArrayType(arr: ResolvedType[L], concreteTypes: List[PropMeta[L]]): F[L#Type]

  // EnumProtocolTerms
  def renderMembers(clsName: String, elems: RenderedEnum[L]): F[Option[L#ObjectDefinition]]
  def encodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]]
  def decodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]]
  def renderClass(clsName: String, tpe: L#Type, elems: RenderedEnum[L]): F[L#ClassDefinition]
  def renderStaticDefns(
      clsName: String,
      tpe: L#Type,
      members: Option[L#ObjectDefinition],
      accessors: List[L#TermName],
      encoder: Option[L#Definition],
      decoder: Option[L#Definition]
  ): F[StaticDefns[L]]
  def buildAccessor(clsName: String, termName: String): F[L#TermSelect]

  // PolyProtocolTerms
  def extractSuperClass(
      swagger: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])]
  ): F[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]]
  def renderSealedTrait(
      className: String,
      params: List[ProtocolParameter[L]],
      discriminator: Discriminator[L],
      parents: List[SuperClass[L]] = Nil,
      children: List[String] = Nil
  ): F[L#Trait]
  def encodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil): F[Option[L#ValueDefinition]]
  def decodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil): F[Option[L#ValueDefinition]]
  def renderADTStaticDefns(
      clsName: String,
      discriminator: Discriminator[L],
      encoder: Option[L#ValueDefinition],
      decoder: Option[L#ValueDefinition]
  ): F[StaticDefns[L]]

  def copy(
      MonadF: Monad[F] = self.MonadF,
      extractConcreteTypes: Either[String, List[PropMeta[L]]] => F[List[PropMeta[L]]] = self.extractConcreteTypes,
      staticProtocolImports: ((List[String]) => F[List[L#Import]]) = self.staticProtocolImports _,
      protocolImports: (() => F[List[L#Import]]) = self.protocolImports _,
      packageObjectImports: (() => F[List[L#Import]]) = self.packageObjectImports _,
      packageObjectContents: (() => F[List[L#Statement]]) = self.packageObjectContents _,
      implicitsObject: () => F[Option[(L#TermName, L#ObjectDefinition)]] = self.implicitsObject _,
      generateSupportDefinitions: (() => F[List[SupportDefinition[L]]]) = self.generateSupportDefinitions _,
      extractProperties: Tracker[Schema[_]] => F[List[(String, Tracker[Schema[_]])]] = self.extractProperties _,
      transformProperty: (
          String,
          List[String],
          List[String],
          List[PropMeta[L]]
      ) => (String, String, Tracker[Schema[_]], ResolvedType[L], PropertyRequirement, Boolean, Option[L#Term]) => F[ProtocolParameter[L]] =
        self.transformProperty _,
      renderDTOClass: (String, List[String], List[ProtocolParameter[L]], List[SuperClass[L]]) => F[L#ClassDefinition] = self.renderDTOClass _,
      decodeModel: (String, List[String], List[String], List[ProtocolParameter[L]], List[SuperClass[L]]) => F[Option[L#ValueDefinition]] = self.decodeModel _,
      encodeModel: (String, List[String], List[ProtocolParameter[L]], List[SuperClass[L]]) => F[Option[L#ValueDefinition]] = self.encodeModel _,
      renderDTOStaticDefns: (String, List[L#TermName], Option[L#ValueDefinition], Option[L#ValueDefinition], List[ProtocolParameter[L]]) => F[StaticDefns[L]] =
        self.renderDTOStaticDefns _,
      extractArrayType: (ResolvedType[L], List[PropMeta[L]]) => F[L#Type] = self.extractArrayType _,
      extractSuperClass: (Tracker[ComposedSchema], List[(String, Tracker[Schema[_]])]) => F[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]] =
        self.extractSuperClass _,
      renderSealedTrait: (String, List[ProtocolParameter[L]], Discriminator[L], List[SuperClass[L]], List[String]) => F[L#Trait] = self.renderSealedTrait _,
      encodeADT: (String, Discriminator[L], List[String]) => F[Option[L#ValueDefinition]] = self.encodeADT _,
      decodeADT: (String, Discriminator[L], List[String]) => F[Option[L#ValueDefinition]] = self.decodeADT _,
      renderADTStaticDefns: (String, Discriminator[L], Option[L#ValueDefinition], Option[L#ValueDefinition]) => F[StaticDefns[L]] = self.renderADTStaticDefns _,
      renderMembers: (String, RenderedEnum[L]) => F[Option[L#ObjectDefinition]] = self.renderMembers _,
      encodeEnum: (String, L#Type) => F[Option[L#Definition]] = self.encodeEnum _,
      decodeEnum: (String, L#Type) => F[Option[L#Definition]] = self.decodeEnum _,
      renderClass: (String, L#Type, RenderedEnum[L]) => F[L#ClassDefinition] = self.renderClass _,
      renderStaticDefns: (String, L#Type, Option[L#ObjectDefinition], List[L#TermName], Option[L#Definition], Option[L#Definition]) => F[StaticDefns[L]] =
        self.renderStaticDefns _,
      buildAccessor: (String, String) => F[L#TermSelect] = self.buildAccessor _
  ): ProtocolTerms[L, F] = {
    val newMonadF                     = MonadF
    val newExtractConcreteTypes       = extractConcreteTypes
    val newStaticProtocolImports      = staticProtocolImports
    val newProtocolImports            = protocolImports
    val newPackageObjectImports       = packageObjectImports
    val newPackageObjectContents      = packageObjectContents
    val newImplicitsObject            = implicitsObject
    val newGenerateSupportDefinitions = generateSupportDefinitions

    val newExtractProperties    = extractProperties
    val newTransformProperty    = transformProperty
    val newRenderDTOClass       = renderDTOClass
    val newDecodeModel          = decodeModel
    val newEncodeModel          = encodeModel
    val newRenderDTOStaticDefns = renderDTOStaticDefns

    val newExtractArrayType = extractArrayType

    val newExtractSuperClass    = extractSuperClass
    val newRenderSealedTrait    = renderSealedTrait
    val newEncodeADT            = encodeADT
    val newDecodeADT            = decodeADT
    val newRenderADTStaticDefns = renderADTStaticDefns

    val newRenderMembers     = renderMembers
    val newEncodeEnum        = encodeEnum
    val newDecodeEnum        = decodeEnum
    val newRenderClass       = renderClass
    val newRenderStaticDefns = renderStaticDefns
    val newBuildAccessor     = buildAccessor

    new ProtocolTerms[L, F] {
      def MonadF                                                          = newMonadF
      def extractConcreteTypes(models: Either[String, List[PropMeta[L]]]) = newExtractConcreteTypes(models)
      def staticProtocolImports(pkgName: List[String])                    = newStaticProtocolImports(pkgName)
      def protocolImports()                                               = newProtocolImports()
      def packageObjectImports()                                          = newPackageObjectImports()
      def packageObjectContents()                                         = newPackageObjectContents()
      def implicitsObject()                                               = newImplicitsObject()
      def generateSupportDefinitions()                                    = newGenerateSupportDefinitions()

      def extractProperties(swagger: Tracker[Schema[_]]) = newExtractProperties(swagger)
      def transformProperty(
          clsName: String,
          dtoPackage: List[String],
          supportPackage: List[String],
          concreteTypes: List[PropMeta[L]]
      )(
          name: String,
          fieldName: String,
          prop: Tracker[Schema[_]],
          meta: ResolvedType[L],
          requirement: PropertyRequirement,
          isCustomType: Boolean,
          defaultValue: Option[L#Term]
      ) =
        newTransformProperty(clsName, dtoPackage, supportPackage, concreteTypes)(
          name,
          fieldName,
          prop,
          meta,
          requirement,
          isCustomType,
          defaultValue
        )
      def renderDTOClass(clsName: String, supportPackage: List[String], terms: List[ProtocolParameter[L]], parents: List[SuperClass[L]] = Nil) =
        newRenderDTOClass(clsName, supportPackage, terms, parents)
      def encodeModel(
          clsName: String,
          dtoPackage: List[String],
          params: List[ProtocolParameter[L]],
          parents: List[SuperClass[L]] = Nil
      ) =
        newEncodeModel(clsName, dtoPackage, params, parents)
      def decodeModel(
          clsName: String,
          dtoPackage: List[String],
          supportPackage: List[String],
          params: List[ProtocolParameter[L]],
          parents: List[SuperClass[L]] = Nil
      ) =
        newDecodeModel(clsName, dtoPackage, supportPackage, params, parents)

      def renderDTOStaticDefns(
          clsName: String,
          deps: List[L#TermName],
          encoder: Option[L#ValueDefinition],
          decoder: Option[L#ValueDefinition],
          params: List[ProtocolParameter[L]]
      ) =
        newRenderDTOStaticDefns(clsName, deps, encoder, decoder, params)

      def extractArrayType(arr: ResolvedType[L], concreteTypes: List[PropMeta[L]]) = newExtractArrayType(arr, concreteTypes)

      def extractSuperClass(swagger: Tracker[ComposedSchema], definitions: List[(String, Tracker[Schema[_]])]) = newExtractSuperClass(swagger, definitions)
      def renderSealedTrait(
          className: String,
          params: List[ProtocolParameter[L]],
          discriminator: Discriminator[L],
          parents: List[SuperClass[L]] = Nil,
          children: List[String] = Nil
      ) = newRenderSealedTrait(className, params, discriminator, parents, children)
      def encodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil) = newEncodeADT(clsName, discriminator, children)
      def decodeADT(clsName: String, discriminator: Discriminator[L], children: List[String] = Nil) = newDecodeADT(clsName, discriminator, children)
      def renderADTStaticDefns(clsName: String, discriminator: Discriminator[L], encoder: Option[L#ValueDefinition], decoder: Option[L#ValueDefinition]) =
        newRenderADTStaticDefns(clsName, discriminator, encoder, decoder)

      def renderMembers(clsName: String, elems: RenderedEnum[L])            = newRenderMembers(clsName, elems)
      def encodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]] = newEncodeEnum(clsName, tpe)
      def decodeEnum(clsName: String, tpe: L#Type): F[Option[L#Definition]] = newDecodeEnum(clsName, tpe)
      def renderClass(clsName: String, tpe: L#Type, elems: RenderedEnum[L]) = newRenderClass(clsName, tpe, elems)
      def renderStaticDefns(
          clsName: String,
          tpe: L#Type,
          members: Option[L#ObjectDefinition],
          accessors: List[L#TermName],
          encoder: Option[L#Definition],
          decoder: Option[L#Definition]
      ): F[StaticDefns[L]] = newRenderStaticDefns(clsName, tpe, members, accessors, encoder, decoder)
      def buildAccessor(clsName: String, termName: String) = newBuildAccessor(clsName, termName)
    }
  }
}
