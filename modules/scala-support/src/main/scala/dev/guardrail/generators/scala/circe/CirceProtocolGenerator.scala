package dev.guardrail.generators.scala.circe

import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, _ }
import _root_.io.swagger.v3.oas.models.{ Components, OpenAPI }
import cats.Foldable
import cats.data.{ NonEmptyList, NonEmptyVector }
import cats.syntax.all._
import scala.jdk.CollectionConverters._
import scala.meta.{ Defn, _ }
import scala.reflect.runtime.universe.typeTag

import dev.guardrail.core
import dev.guardrail.core.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, DataRedaction, Default, EmptyValueIsNull }
import dev.guardrail.core.implicits._
import dev.guardrail.core.resolvers.ModelResolver
import dev.guardrail.core.{ DataRedacted, DataVisible, EmptyIsEmpty, EmptyIsNull, LiteralRawType, Mappish, ReifiedRawType, SupportDefinition, Tracker }
import dev.guardrail.generators.ProtocolGenerator.{ WrapEnumSchema, wrapNumberEnumSchema, wrapObjectEnumSchema, wrapStringEnumSchema }
import dev.guardrail.generators.protocol.{ ClassChild, ClassHierarchy, ClassParent }
import dev.guardrail.generators.scala.circe.CirceProtocolGenerator.WithValidations
import dev.guardrail.generators.scala.{ CirceModelGenerator, ScalaLanguage }
import dev.guardrail.generators.spi.{ ModuleLoadResult, ProtocolGeneratorLoader }
import dev.guardrail.generators.{ ProtocolDefinitions, RawParameterName }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.PropertyRequirement
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.{
  CollectionsLibTerms,
  HeldEnum,
  IntHeldEnum,
  LanguageTerms,
  LongHeldEnum,
  OpenAPITerms,
  ProtocolTerms,
  RenderedEnum,
  RenderedIntEnum,
  RenderedLongEnum,
  RenderedStringEnum,
  StringHeldEnum
}
import dev.guardrail.{ Target, UserError }

class CirceProtocolGeneratorLoader extends ProtocolGeneratorLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  val apply =
    ModuleLoadResult.forProduct1(ProtocolGeneratorLoader.label -> Seq(CirceModelGenerator.mapping))(circeVersion => CirceProtocolGenerator(circeVersion))
}

object CirceProtocolGenerator {
  def apply(circeVersion: CirceModelGenerator): ProtocolTerms[ScalaLanguage, Target] =
    new CirceProtocolGenerator(circeVersion, WithValidations.ignore)
  def withValidations(circeVersion: CirceModelGenerator, applyValidations: WithValidations): ProtocolTerms[ScalaLanguage, Target] =
    new CirceProtocolGenerator(circeVersion, applyValidations)

  @FunctionalInterface
  trait WithValidations {
    def apply(className: String, tpe: Type, property: Tracker[Schema[_]]): Target[Type]
  }
  object WithValidations {
    val ignore: WithValidations = (className: String, tpe: Type, property: Tracker[Schema[_]]) => Target.pure(tpe)
  }
}

class CirceProtocolGenerator private (circeVersion: CirceModelGenerator, applyValidations: WithValidations) extends ProtocolTerms[ScalaLanguage, Target] {
  override def fromSpec(
      spec: Tracker[OpenAPI],
      dtoPackage: List[String],
      supportPackage: NonEmptyList[String],
      defaultPropertyRequirement: PropertyRequirement
  )(implicit
      F: FrameworkTerms[ScalaLanguage, Target],
      P: ProtocolTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[ProtocolDefinitions[ScalaLanguage]] = {
    import Cl._
    import Sc._

    val components  = spec.downField("components", _.getComponents())
    val definitions = components.flatDownField("schemas", _.getSchemas()).indexedCosequence
    Sw.log.function("ProtocolGenerator.fromSpec")(for {
      (hierarchies, definitionsWithoutPoly) <- groupHierarchies(definitions)

      concreteTypes <- PropMeta.extractConcreteTypes[ScalaLanguage, Target](definitions.value, components)
      polyADTs <- hierarchies.traverse(fromPoly(_, concreteTypes, definitions.value, dtoPackage, supportPackage.toList, defaultPropertyRequirement, components))
      prefixes <- vendorPrefixes()
      elems <- definitionsWithoutPoly.traverse { case (clsName, model) =>
        model
          .refine { case c: ComposedSchema => c }(comp =>
            for {
              formattedClsName <- formatTypeName(clsName)
              parents <- extractParents(comp, definitions.value, concreteTypes, dtoPackage, supportPackage.toList, defaultPropertyRequirement, components)
              model <- fromModel(
                clsName = NonEmptyList.of(formattedClsName),
                model = comp,
                parents = parents,
                concreteTypes = concreteTypes,
                definitions = definitions.value,
                dtoPackage = dtoPackage,
                supportPackage = supportPackage.toList,
                defaultPropertyRequirement = defaultPropertyRequirement,
                components = components
              )
              alias <- modelTypeAlias(formattedClsName, comp, components)
            } yield model.getOrElse(alias)
          )
          .orRefine { case a: ArraySchema => a }(arr =>
            for {
              formattedClsName <- formatTypeName(clsName)
              array            <- fromArray(formattedClsName, arr, concreteTypes, components)
            } yield array
          )
          .orRefine { case o: ObjectSchema => o }(m =>
            for {
              formattedClsName <- formatTypeName(clsName)
              enum             <- fromEnum[Object](formattedClsName, m, dtoPackage, components)
              model <- fromModel(
                NonEmptyList.of(formattedClsName),
                m,
                List.empty,
                concreteTypes,
                definitions.value,
                dtoPackage,
                supportPackage.toList,
                defaultPropertyRequirement,
                components
              )
              alias <- modelTypeAlias(formattedClsName, m, components)
            } yield enum.orElse(model).getOrElse(alias)
          )
          .orRefine { case x: StringSchema => x }(x =>
            for {
              formattedClsName <- formatTypeName(clsName)
              enum             <- fromEnum(formattedClsName, x, dtoPackage, components)
              model <- fromModel(
                NonEmptyList.of(formattedClsName),
                x,
                List.empty,
                concreteTypes,
                definitions.value,
                dtoPackage,
                supportPackage.toList,
                defaultPropertyRequirement,
                components
              )

              (declType, _) <- ModelResolver.determineTypeName[ScalaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
              alias         <- typeAlias(formattedClsName, declType)
            } yield enum.orElse(model).getOrElse(alias)
          )
          .orRefine { case x: IntegerSchema => x }(x =>
            for {
              formattedClsName <- formatTypeName(clsName)
              enum             <- fromEnum(formattedClsName, x, dtoPackage, components)
              model <- fromModel(
                NonEmptyList.of(formattedClsName),
                x,
                List.empty,
                concreteTypes,
                definitions.value,
                dtoPackage,
                supportPackage.toList,
                defaultPropertyRequirement,
                components
              )
              (declType, _) <- ModelResolver.determineTypeName[ScalaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
              alias         <- typeAlias(formattedClsName, declType)
            } yield enum.orElse(model).getOrElse(alias)
          )
          .valueOr(x =>
            for {
              formattedClsName <- formatTypeName(clsName)
              (declType, _)    <- ModelResolver.determineTypeName[ScalaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
              res              <- typeAlias(formattedClsName, declType)
            } yield res
          )
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()
      implicitsObject   <- implicitsObject()

      polyADTElems <- ProtocolElems.resolve[ScalaLanguage, Target](polyADTs)
      strictElems  <- ProtocolElems.resolve[ScalaLanguage, Target](elems)
    } yield ProtocolDefinitions[ScalaLanguage](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents, implicitsObject))
  }

  private[this] def isFile(typeName: String, format: Option[String]): Boolean =
    (typeName, format) match {
      case ("string", Some("binary")) => true
      case ("file", _)                => true
      case ("binary", _)              => true
      case _                          => false
    }

  private[this] def getRequiredFieldsRec(root: Tracker[Schema[_]]): List[String] = {
    @scala.annotation.tailrec
    def work(values: List[Tracker[Schema[_]]], acc: List[String]): List[String] = {
      val required: List[String] = values.flatMap(_.downField("required", _.getRequired()).unwrapTracker)
      val next: List[Tracker[Schema[_]]] =
        for {
          a <- values
          b <- a.refine { case x: ComposedSchema => x }(_.downField("allOf", _.getAllOf())).toOption.toList
          c <- b.indexedDistribute
        } yield c

      val newRequired = acc ++ required

      next match {
        case next @ (_ :: _) => work(next, newRequired)
        case Nil             => newRequired
      }
    }
    work(List(root), Nil)
  }

  private[this] def fromEnum[A](
      clsName: String,
      schema: Tracker[Schema[A]],
      dtoPackage: List[String],
      components: Tracker[Option[Components]]
  )(implicit
      P: ProtocolTerms[ScalaLanguage, Target],
      F: FrameworkTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target],
      wrapEnumSchema: WrapEnumSchema[A]
  ): Target[Either[String, EnumDefinition[ScalaLanguage]]] = {
    import Cl._
    import Sc._
    import Sw._

    def validProg(held: HeldEnum, tpe: scala.meta.Type, fullType: scala.meta.Type): Target[EnumDefinition[ScalaLanguage]] =
      for {
        (pascalValues, wrappedValues) <- held match {
          case StringHeldEnum(value) =>
            for {
              elems <- value.traverse { elem =>
                for {
                  termName  <- formatEnumName(elem)
                  valueTerm <- pureTermName(termName)
                  accessor  <- buildAccessor(clsName, termName)
                } yield (elem, valueTerm, accessor)
              }
              pascalValues  = elems.map(_._2)
              wrappedValues = RenderedStringEnum[ScalaLanguage](elems)
            } yield (pascalValues, wrappedValues)
          case IntHeldEnum(value) =>
            for {
              elems <- value.traverse { elem =>
                for {
                  termName  <- formatEnumName(s"${clsName}${elem}") // TODO: Push this string into LanguageTerms
                  valueTerm <- pureTermName(termName)
                  accessor  <- buildAccessor(clsName, termName)
                } yield (elem, valueTerm, accessor)
              }
              pascalValues  = elems.map(_._2)
              wrappedValues = RenderedIntEnum[ScalaLanguage](elems)
            } yield (pascalValues, wrappedValues)
          case LongHeldEnum(value) =>
            for {
              elems <- value.traverse { elem =>
                for {
                  termName  <- formatEnumName(s"${clsName}${elem}") // TODO: Push this string into LanguageTerms
                  valueTerm <- pureTermName(termName)
                  accessor  <- buildAccessor(clsName, termName)
                } yield (elem, valueTerm, accessor)
              }
              pascalValues  = elems.map(_._2)
              wrappedValues = RenderedLongEnum[ScalaLanguage](elems)
            } yield (pascalValues, wrappedValues)
        }
        members <- renderMembers(clsName, wrappedValues)
        encoder <- encodeEnum(clsName, tpe)
        decoder <- decodeEnum(clsName, tpe)

        defn        <- renderClass(clsName, tpe, wrappedValues)
        staticDefns <- renderStaticDefns(clsName, tpe, members, pascalValues, encoder, decoder)
        classType   <- pureTypeName(clsName)
      } yield EnumDefinition[ScalaLanguage](clsName, classType, fullType, wrappedValues, defn, staticDefns)

    for {
      enum     <- extractEnum(schema.map(wrapEnumSchema))
      prefixes <- vendorPrefixes()
      (tpe, _) <- ModelResolver.determineTypeName[ScalaLanguage, Target](schema, Tracker.cloneHistory(schema, CustomTypeName(schema, prefixes)), components)
      fullType <- selectType(NonEmptyList.ofInitLast(dtoPackage, clsName))
      res      <- enum.traverse(validProg(_, tpe, fullType))
    } yield res
  }

  private[this] def getPropertyRequirement(
      schema: Tracker[Schema[_]],
      isRequired: Boolean,
      defaultPropertyRequirement: PropertyRequirement
  ): PropertyRequirement =
    (for {
      isNullable <- schema.downField("nullable", _.getNullable)
    } yield (isRequired, isNullable) match {
      case (true, None)         => PropertyRequirement.Required
      case (true, Some(false))  => PropertyRequirement.Required
      case (true, Some(true))   => PropertyRequirement.RequiredNullable
      case (false, None)        => defaultPropertyRequirement
      case (false, Some(false)) => PropertyRequirement.Optional
      case (false, Some(true))  => PropertyRequirement.OptionalNullable
    }).unwrapTracker

  /** Handle polymorphic model
    */
  private[this] def fromPoly(
      hierarchy: ClassParent[ScalaLanguage],
      concreteTypes: List[PropMeta[ScalaLanguage]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[ScalaLanguage, Target],
      P: ProtocolTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[ProtocolElems[ScalaLanguage]] = {
    import Cl._
    import Sc._

    def child(hierarchy: ClassHierarchy[ScalaLanguage]): List[String] =
      hierarchy.children.map(_.name) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy[ScalaLanguage]): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.name :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator

    for {
      parents <- hierarchy.model
        .refine[Target[List[SuperClass[ScalaLanguage]]]] { case c: ComposedSchema => c }(
          extractParents(_, definitions, concreteTypes, dtoPackage, supportPackage, defaultPropertyRequirement, components)
        )
        .getOrElse(List.empty[SuperClass[ScalaLanguage]].pure[Target])
      props <- extractProperties(hierarchy.model)
      requiredFields = hierarchy.required ::: hierarchy.children.flatMap(_.required)
      prefixes <- vendorPrefixes()
      params <- props.traverse { case (name, prop) =>
        for {
          typeName <- formatTypeName(name).map(formattedName => NonEmptyList.of(hierarchy.name, formattedName))
          propertyRequirement = getPropertyRequirement(prop, requiredFields.contains(name), defaultPropertyRequirement)
          resolvedType <- ModelResolver
            .propMeta[ScalaLanguage, Target](
              prop,
              components
            ) // TODO: This should be resolved via an alternate mechanism that maintains references all the way through, instead of re-deriving and assuming that references are valid
          defValue  <- defaultValue(typeName, prop, propertyRequirement, definitions)
          fieldName <- formatFieldName(name)
          res <- transformProperty(hierarchy.name, dtoPackage, supportPackage, concreteTypes)(
            name,
            fieldName,
            prop,
            resolvedType,
            propertyRequirement,
            CustomTypeName(prop, prefixes).isDefined,
            defValue
          )
        } yield res
      }
      definition  <- renderSealedTrait(hierarchy.name, params, discriminator, parents, children)
      encoder     <- encodeADT(hierarchy.name, hierarchy.discriminator, children)
      decoder     <- decodeADT(hierarchy.name, hierarchy.discriminator, children)
      staticDefns <- renderADTStaticDefns(hierarchy.name, discriminator, encoder, decoder)
      tpe         <- pureTypeName(hierarchy.name)
      fullType    <- selectType(NonEmptyList.fromList(dtoPackage :+ hierarchy.name).getOrElse(NonEmptyList.of(hierarchy.name)))
    } yield ADT[ScalaLanguage](
      name = hierarchy.name,
      tpe = tpe,
      fullType = fullType,
      trt = definition,
      staticDefns = staticDefns
    )
  }

  private def extractParents(
      elem: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])],
      concreteTypes: List[PropMeta[ScalaLanguage]],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[ScalaLanguage, Target],
      P: ProtocolTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[List[SuperClass[ScalaLanguage]]] = {
    import Sc._

    for {
      a <- extractSuperClass(elem, definitions)
      supper <- a.flatTraverse { case (clsName, _extends, interfaces) =>
        val concreteInterfacesWithClass = for {
          interface      <- interfaces
          (cls, tracker) <- definitions
          result <- tracker
            .refine[Tracker[Schema[_]]] {
              case x: ComposedSchema if interface.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/${cls}")) => x
            }(
              identity _
            )
            .orRefine { case x: Schema[_] if interface.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/${cls}")) => x }(identity _)
            .toOption
        } yield cls -> result
        val (_, concreteInterfaces) = concreteInterfacesWithClass.unzip
        val classMapping = (for {
          (cls, schema) <- concreteInterfacesWithClass
          (name, _)     <- schema.downField("properties", _.getProperties).indexedDistribute.value
        } yield (name, cls)).toMap
        for {
          _extendsProps <- extractProperties(_extends)
          requiredFields = getRequiredFieldsRec(_extends) ++ concreteInterfaces.flatMap(getRequiredFieldsRec)
          _withProps <- concreteInterfaces.traverse(extractProperties)
          props = _extendsProps ++ _withProps.flatten
          (params, _) <- prepareProperties(
            NonEmptyList.of(clsName),
            classMapping,
            props,
            requiredFields,
            concreteTypes,
            definitions,
            dtoPackage,
            supportPackage,
            defaultPropertyRequirement,
            components
          )
          interfacesCls = interfaces.flatMap(_.downField("$ref", _.get$ref).unwrapTracker.map(_.split("/").last))
          tpe <- parseTypeName(clsName)

          discriminators <- (_extends :: concreteInterfaces).flatTraverse(
            _.refine[Target[List[Discriminator[ScalaLanguage]]]] { case m: ObjectSchema => m }(m =>
              Discriminator.fromSchema[ScalaLanguage, Target](m).map(_.toList)
            )
              .getOrElse(List.empty[Discriminator[ScalaLanguage]].pure[Target])
          )
        } yield tpe
          .map(
            SuperClass[ScalaLanguage](
              clsName,
              _,
              interfacesCls,
              params,
              discriminators
            )
          )
          .toList
      }

    } yield supper
  }

  private[this] def fromModel(
      clsName: NonEmptyList[String],
      model: Tracker[Schema[_]],
      parents: List[SuperClass[ScalaLanguage]],
      concreteTypes: List[PropMeta[ScalaLanguage]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[ScalaLanguage, Target],
      P: ProtocolTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[Either[String, ClassDefinition[ScalaLanguage]]] = {
    import Sc._

    for {
      props <- extractProperties(model)
      requiredFields = getRequiredFieldsRec(model)
      (params, nestedDefinitions) <- prepareProperties(
        clsName,
        Map.empty,
        props,
        requiredFields,
        concreteTypes,
        definitions,
        dtoPackage,
        supportPackage,
        defaultPropertyRequirement,
        components
      )
      encoder     <- encodeModel(clsName.last, dtoPackage, params, parents)
      decoder     <- decodeModel(clsName.last, dtoPackage, supportPackage, params, parents)
      tpe         <- parseTypeName(clsName.last)
      fullType    <- selectType(dtoPackage.foldRight(clsName)((x, xs) => xs.prepend(x)))
      staticDefns <- renderDTOStaticDefns(clsName.last, List.empty, encoder, decoder, params)
      nestedClasses <- nestedDefinitions.flatTraverse {
        case classDefinition: ClassDefinition[ScalaLanguage] =>
          for {
            widenClass          <- widenClassDefinition(classDefinition.cls)
            companionTerm       <- pureTermName(classDefinition.name)
            companionDefinition <- wrapToObject(companionTerm, classDefinition.staticDefns.extraImports, classDefinition.staticDefns.definitions)
            widenCompanion      <- companionDefinition.traverse(widenObjectDefinition)
          } yield List(widenClass) ++ widenCompanion.fold(classDefinition.staticDefns.definitions)(List(_))
        case enumDefinition: EnumDefinition[ScalaLanguage] =>
          for {
            widenClass          <- widenClassDefinition(enumDefinition.cls)
            companionTerm       <- pureTermName(enumDefinition.name)
            companionDefinition <- wrapToObject(companionTerm, enumDefinition.staticDefns.extraImports, enumDefinition.staticDefns.definitions)
            widenCompanion      <- companionDefinition.traverse(widenObjectDefinition)
          } yield List(widenClass) ++ widenCompanion.fold(enumDefinition.staticDefns.definitions)(List(_))
      }
      defn <- renderDTOClass(clsName.last, supportPackage, params, parents)
    } yield {
      val finalStaticDefns = staticDefns.copy(definitions = staticDefns.definitions ++ nestedClasses)
      if (parents.isEmpty && props.isEmpty) Left("Entity isn't model"): Either[String, ClassDefinition[ScalaLanguage]]
      else tpe.toRight("Empty entity name").map(ClassDefinition[ScalaLanguage](clsName.last, _, fullType, defn, finalStaticDefns, parents))
    }

  }

  private def prepareProperties(
      clsName: NonEmptyList[String],
      propertyToTypeLookup: Map[String, String],
      props: List[(String, Tracker[Schema[_]])],
      requiredFields: List[String],
      concreteTypes: List[PropMeta[ScalaLanguage]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[ScalaLanguage, Target],
      P: ProtocolTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[(List[ProtocolParameter[ScalaLanguage]], List[NestedProtocolElems[ScalaLanguage]])] = {
    import Cl._
    import Sc._
    def getClsName(name: String): NonEmptyList[String] = propertyToTypeLookup.get(name).map(NonEmptyList.of(_)).getOrElse(clsName)

    def processProperty(name: String, schema: Tracker[Schema[_]]): Target[Option[Either[String, NestedProtocolElems[ScalaLanguage]]]] =
      for {
        nestedClassName <- formatTypeName(name).map(formattedName => getClsName(name).append(formattedName))
        defn <- schema
          .refine[Target[Option[Either[String, NestedProtocolElems[ScalaLanguage]]]]] { case x: ObjectSchema => x }(o =>
            for {
              defn <- fromModel(
                nestedClassName,
                o,
                List.empty,
                concreteTypes,
                definitions,
                dtoPackage,
                supportPackage,
                defaultPropertyRequirement,
                components
              )
            } yield Option(defn)
          )
          .orRefine { case o: ComposedSchema => o }(o =>
            for {
              parents <- extractParents(o, definitions, concreteTypes, dtoPackage, supportPackage, defaultPropertyRequirement, components)
              maybeClassDefinition <- fromModel(
                nestedClassName,
                o,
                parents,
                concreteTypes,
                definitions,
                dtoPackage,
                supportPackage,
                defaultPropertyRequirement,
                components
              )
            } yield Option(maybeClassDefinition)
          )
          .orRefine { case a: ArraySchema => a }(_.downField("items", _.getItems()).indexedCosequence.flatTraverse(processProperty(name, _)))
          .orRefine { case s: StringSchema if Option(s.getEnum).map(_.asScala).exists(_.nonEmpty) => s }(s =>
            fromEnum(nestedClassName.last, s, dtoPackage, components).map(Option(_))
          )
          .getOrElse(Option.empty[Either[String, NestedProtocolElems[ScalaLanguage]]].pure[Target])
      } yield defn

    for {
      prefixes <- vendorPrefixes()
      paramsAndNestedDefinitions <- props.traverse[Target, (Tracker[ProtocolParameter[ScalaLanguage]], Option[NestedProtocolElems[ScalaLanguage]])] {
        case (name, schema) =>
          for {
            typeName              <- formatTypeName(name).map(formattedName => getClsName(name).append(formattedName))
            tpe                   <- selectType(typeName)
            maybeNestedDefinition <- processProperty(name, schema)
            resolvedType          <- ModelResolver.propMetaWithName[ScalaLanguage, Target](tpe, schema, components)
            propertyRequirement = getPropertyRequirement(schema, requiredFields.contains(name), defaultPropertyRequirement)
            defValue  <- defaultValue(typeName, schema, propertyRequirement, definitions)
            fieldName <- formatFieldName(name)
            parameter <- transformProperty(getClsName(name).last, dtoPackage, supportPackage, concreteTypes)(
              name,
              fieldName,
              schema,
              resolvedType,
              propertyRequirement,
              CustomTypeName(schema, prefixes).isDefined,
              defValue
            )
          } yield (Tracker.cloneHistory(schema, parameter), maybeNestedDefinition.flatMap(_.toOption))
      }
      (params, nestedDefinitions) = paramsAndNestedDefinitions.unzip
      deduplicatedParams <- deduplicateParams(params)
      unconflictedParams <- fixConflictingNames(deduplicatedParams)
    } yield (unconflictedParams, nestedDefinitions.flatten)
  }

  private def deduplicateParams(
      params: List[Tracker[ProtocolParameter[ScalaLanguage]]]
  )(implicit Sw: OpenAPITerms[ScalaLanguage, Target], Sc: LanguageTerms[ScalaLanguage, Target]): Target[List[ProtocolParameter[ScalaLanguage]]] = {
    import Sc._
    Foldable[List]
      .foldLeftM[Target, Tracker[ProtocolParameter[ScalaLanguage]], List[ProtocolParameter[ScalaLanguage]]](
        params,
        List.empty[ProtocolParameter[ScalaLanguage]]
      ) { (s, ta) =>
        val a = ta.unwrapTracker
        s.find(p => p.name == a.name) match {
          case None => (a :: s).pure[Target]
          case Some(duplicate) =>
            for {
              newDefaultValue <- findCommonDefaultValue(ta.showHistory, a.defaultValue, duplicate.defaultValue)
              newRawType      <- findCommonRawType(ta.showHistory, a.rawType, duplicate.rawType)
            } yield {
              val emptyToNull        = if (Set(a.emptyToNull, duplicate.emptyToNull).contains(EmptyIsNull)) EmptyIsNull else EmptyIsEmpty
              val redactionBehaviour = if (Set(a.dataRedaction, duplicate.dataRedaction).contains(DataRedacted)) DataRedacted else DataVisible
              val mergedParameter = ProtocolParameter[ScalaLanguage](
                a.term,
                a.baseType,
                a.name,
                a.dep,
                newRawType,
                a.readOnlyKey.orElse(duplicate.readOnlyKey),
                emptyToNull,
                redactionBehaviour,
                a.propertyRequirement,
                newDefaultValue,
                a.propertyValidation
              )
              mergedParameter :: s.filter(_.name != a.name)
            }
        }
      }
      .map(_.reverse)
  }

  private def fixConflictingNames(
      params: List[ProtocolParameter[ScalaLanguage]]
  )(implicit Lt: LanguageTerms[ScalaLanguage, Target]): Target[List[ProtocolParameter[ScalaLanguage]]] = {
    import Lt._
    for {
      paramsWithNames <- params.traverse(param => extractTermNameFromParam(param.term).map((_, param)))
      counts = paramsWithNames.groupBy(_._1).view.mapValues(_.length).toMap
      newParams <- paramsWithNames.traverse { case (name, param) =>
        if (counts.getOrElse(name, 0) > 1) {
          for {
            newTermName    <- pureTermName(param.name.value)
            newMethodParam <- alterMethodParameterName(param.term, newTermName)
          } yield ProtocolParameter(
            newMethodParam,
            param.baseType,
            param.name,
            param.dep,
            param.rawType,
            param.readOnlyKey,
            param.emptyToNull,
            param.dataRedaction,
            param.propertyRequirement,
            param.defaultValue,
            param.propertyValidation
          )
        } else {
          param.pure[Target]
        }
      }
    } yield newParams
  }

  private def modelTypeAlias(clsName: String, abstractModel: Tracker[Schema[_]], components: Tracker[Option[Components]])(implicit
      Fw: FrameworkTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[ProtocolElems[ScalaLanguage]] = {
    import Cl._
    import Fw._
    val model: Option[Tracker[ObjectSchema]] = abstractModel
      .refine[Option[Tracker[ObjectSchema]]] { case m: ObjectSchema => m }(x => Option(x))
      .orRefine { case m: ComposedSchema => m }(
        _.downField("allOf", _.getAllOf()).indexedCosequence
          .get(1)
          .flatMap(
            _.refine { case o: ObjectSchema => o }(Option.apply)
              .orRefineFallback(_ => None)
          )
      )
      .orRefineFallback(_ => None)
    for {
      tpe <- model.fold[Target[scala.meta.Type]](objectType(None)) { m =>
        for {
          prefixes      <- vendorPrefixes()
          (declType, _) <- ModelResolver.determineTypeName[ScalaLanguage, Target](m, Tracker.cloneHistory(m, CustomTypeName(m, prefixes)), components)
        } yield declType
      }
      res <- typeAlias(clsName, tpe)
    } yield res
  }

  private def plainTypeAlias(
      clsName: String
  )(implicit Fw: FrameworkTerms[ScalaLanguage, Target], Sc: LanguageTerms[ScalaLanguage, Target]): Target[ProtocolElems[ScalaLanguage]] = {
    import Fw._
    for {
      tpe <- objectType(None)
      res <- typeAlias(clsName, tpe)
    } yield res
  }

  private def typeAlias(clsName: String, tpe: scala.meta.Type): Target[ProtocolElems[ScalaLanguage]] =
    (RandomType[ScalaLanguage](clsName, tpe): ProtocolElems[ScalaLanguage]).pure[Target]

  private def fromArray(clsName: String, arr: Tracker[ArraySchema], concreteTypes: List[PropMeta[ScalaLanguage]], components: Tracker[Option[Components]])(
      implicit
      F: FrameworkTerms[ScalaLanguage, Target],
      P: ProtocolTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[ProtocolElems[ScalaLanguage]] =
    for {
      deferredTpe <- ModelResolver.modelMetaType[ScalaLanguage, Target](arr, components)
      tpe         <- extractArrayType(deferredTpe, concreteTypes)
      ret         <- typeAlias(clsName, tpe)
    } yield ret

  /** returns objects grouped into hierarchies
    */
  private def groupHierarchies(
      definitions: Mappish[List, String, Tracker[Schema[_]]]
  )(implicit
      Sc: LanguageTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[(List[ClassParent[ScalaLanguage]], List[(String, Tracker[Schema[_]])])] = {

    def firstInHierarchy(model: Tracker[Schema[_]]): Option[Tracker[ObjectSchema]] =
      model
        .refine { case x: ComposedSchema => x } { elem =>
          definitions.value
            .collectFirst {
              case (clsName, element)
                  if elem.downField("allOf", _.getAllOf).exists(_.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/$clsName"))) =>
                element
            }
            .flatMap(
              _.refine { case x: ComposedSchema => x }(firstInHierarchy)
                .orRefine { case o: ObjectSchema => o }(x => Option(x))
                .getOrElse(None)
            )
        }
        .getOrElse(None)

    def children(cls: String): List[ClassChild[ScalaLanguage]] = definitions.value.flatMap { case (clsName, comp) =>
      comp
        .refine { case x: ComposedSchema => x }(comp =>
          if (
            comp
              .downField("allOf", _.getAllOf())
              .exists(x => x.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/$cls")))
          ) {
            Some(ClassChild(clsName, comp, children(clsName), getRequiredFieldsRec(comp)))
          } else None
        )
        .getOrElse(None)
    }

    def classHierarchy(cls: String, model: Tracker[Schema[_]]): Target[Option[ClassParent[ScalaLanguage]]] =
      model
        .refine { case c: ComposedSchema => c }(c =>
          firstInHierarchy(c)
            .fold(Option.empty[Discriminator[ScalaLanguage]].pure[Target])(Discriminator.fromSchema[ScalaLanguage, Target])
            .map(_.map((_, getRequiredFieldsRec(c))))
        )
        .orRefine { case x: Schema[_] => x }(m => Discriminator.fromSchema[ScalaLanguage, Target](m).map(_.map((_, getRequiredFieldsRec(m)))))
        .getOrElse(Option.empty[(Discriminator[ScalaLanguage], List[String])].pure[Target])
        .map(_.map { case (discriminator, reqFields) => ClassParent(cls, model, children(cls), discriminator, reqFields) })

    Sw.log.function("groupHierarchies")(
      definitions.value
        .traverse { case (cls, model) =>
          for {
            hierarchy <- classHierarchy(cls, model)
          } yield hierarchy.filterNot(_.children.isEmpty).toLeft((cls, model))
        }
        .map(_.partitionEither[ClassParent[ScalaLanguage], (String, Tracker[Schema[_]])](identity))
    )
  }

  private def defaultValue(
      name: NonEmptyList[String],
      schema: Tracker[Schema[_]],
      requirement: PropertyRequirement,
      definitions: List[(String, Tracker[Schema[_]])]
  )(implicit
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target]
  ): Target[Option[scala.meta.Term]] = {
    import Sc._
    import Cl._
    val empty = Option.empty[scala.meta.Term].pure[Target]
    schema.downField("$ref", _.get$ref()).indexedDistribute match {
      case Some(ref) =>
        definitions
          .collectFirst {
            case (cls, refSchema) if ref.unwrapTracker.endsWith(s"/$cls") =>
              defaultValue(NonEmptyList.of(cls), refSchema, requirement, definitions)
          }
          .getOrElse(empty)
      case None =>
        schema
          .refine { case map: MapSchema if requirement == PropertyRequirement.Required || requirement == PropertyRequirement.RequiredNullable => map }(map =>
            for {
              prefixes <- vendorPrefixes()
              result   <- CustomMapTypeName(map, prefixes).fold(emptyMap().map(Option(_)))(_ => empty)
            } yield result
          )
          .orRefine { case arr: ArraySchema if requirement == PropertyRequirement.Required || requirement == PropertyRequirement.RequiredNullable => arr }(
            arr =>
              for {
                prefixes <- vendorPrefixes()
                result   <- CustomArrayTypeName(arr, prefixes).fold(emptyArray().map(Option(_)))(_ => empty)
              } yield result
          )
          .orRefine { case p: BooleanSchema => p }(p => Default(p).extract[Boolean].fold(empty)(litBoolean(_).map(Some(_))))
          .orRefine { case p: NumberSchema if p.getFormat == "double" => p }(p => Default(p).extract[Double].fold(empty)(litDouble(_).map(Some(_))))
          .orRefine { case p: NumberSchema if p.getFormat == "float" => p }(p => Default(p).extract[Float].fold(empty)(litFloat(_).map(Some(_))))
          .orRefine { case p: IntegerSchema if p.getFormat == "int32" => p }(p => Default(p).extract[Int].fold(empty)(litInt(_).map(Some(_))))
          .orRefine { case p: IntegerSchema if p.getFormat == "int64" => p }(p => Default(p).extract[Long].fold(empty)(litLong(_).map(Some(_))))
          .orRefine { case p: StringSchema if Option(p.getEnum).map(_.asScala).exists(_.nonEmpty) => p }(p =>
            Default(p).extract[String] match {
              case Some(defaultEnumValue) =>
                for {
                  enumName <- formatEnumName(defaultEnumValue)
                  result   <- selectTerm(name.append(enumName))
                } yield Some(result)
              case None => empty
            }
          )
          .orRefine { case p: StringSchema => p }(p => Default(p).extract[String].fold(empty)(litString(_).map(Some(_))))
          .getOrElse(empty)
    }
  }

  private def suffixClsName(prefix: String, clsName: String): Pat.Var = Pat.Var(Term.Name(s"${prefix}${clsName}"))

  private def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[ScalaLanguage]])(f: Type => Type): Option[Type] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  private def renderMembers(clsName: String, elems: RenderedEnum[ScalaLanguage]) = {
    val fields = elems match {
      case RenderedStringEnum(elems) =>
        elems.map { case (value, termName, _) =>
          (termName, Lit.String(value))
        }
      case RenderedIntEnum(elems) =>
        elems.map { case (value, termName, _) =>
          (termName, Lit.Int(value))
        }
      case RenderedLongEnum(elems) =>
        elems.map { case (value, termName, _) =>
          (termName, Lit.Long(value))
        }
    }

    Target.pure(Some(q"""
            object members {
              ..${fields.map { case (termName, lit) => q"""case object ${termName} extends ${Type.Name(clsName)}(${lit})""" }}
            }
          """))
  }

  private def encodeEnum(clsName: String, tpe: Type): Target[Option[Defn]] =
    Target.pure(Some(q"""
          implicit val ${suffixClsName("encode", clsName)}: _root_.io.circe.Encoder[${Type.Name(clsName)}] =
            _root_.io.circe.Encoder[${tpe}].contramap(_.value)
        """))

  private def decodeEnum(clsName: String, tpe: Type): Target[Option[Defn]] =
    Target.pure(Some(q"""
      implicit val ${suffixClsName("decode", clsName)}: _root_.io.circe.Decoder[${Type.Name(clsName)}] =
        _root_.io.circe.Decoder[${tpe}].emap(value => from(value).toRight(${Term
        .Interpolate(Term.Name("s"), List(Lit.String(""), Lit.String(s" not a member of ${clsName}")), List(Term.Name("value")))}))
    """))

  private def renderClass(clsName: String, tpe: scala.meta.Type, elems: RenderedEnum[ScalaLanguage]) =
    Target.pure(q"""
      sealed abstract class ${Type.Name(clsName)}(val value: ${tpe}) extends _root_.scala.Product with _root_.scala.Serializable {
        override def toString: String = value.toString
      }
    """)

  private def renderStaticDefns(
      clsName: String,
      tpe: scala.meta.Type,
      members: Option[scala.meta.Defn.Object],
      accessors: List[scala.meta.Term.Name],
      encoder: Option[scala.meta.Defn],
      decoder: Option[scala.meta.Defn]
  ): Target[StaticDefns[ScalaLanguage]] = {
    val longType = Type.Name(clsName)
    val terms: List[Defn.Val] = accessors.map { pascalValue =>
      q"val ${Pat.Var(pascalValue)}: ${longType} = members.${pascalValue}"
    }.toList
    val values: Defn.Val = q"val values = _root_.scala.Vector(..$accessors)"
    val implicits: List[Defn.Val] = List(
      q"implicit val ${Pat.Var(Term.Name(s"show${clsName}"))}: Show[${longType}] = Show[${tpe}].contramap[${longType}](_.value)"
    )
    Target.pure(
      StaticDefns[ScalaLanguage](
        className = clsName,
        extraImports = List.empty[Import],
        definitions = members.toList ++
          terms ++
          List(Some(values), encoder, decoder).flatten ++
          implicits ++
          List(
            q"def from(value: ${tpe}): _root_.scala.Option[${longType}] = values.find(_.value == value)",
            q"implicit val order: cats.Order[${longType}] = cats.Order.by[${longType}, Int](values.indexOf)"
          )
      )
    )
  }

  override def buildAccessor(clsName: String, termName: String) =
    Target.pure(q"${Term.Name(clsName)}.${Term.Name(termName)}")

  private def extractProperties(spec: Tracker[Schema[_]]) =
    spec
      .refine[Target[List[(String, Tracker[Schema[_]])]]] { case o: ObjectSchema => o }(m =>
        Target.pure(m.downField("properties", _.getProperties()).indexedCosequence.value)
      )
      .orRefine { case c: ComposedSchema => c } { comp =>
        val extractedProps =
          comp
            .downField("allOf", _.getAllOf())
            .indexedDistribute
            .flatMap(_.downField("properties", _.getProperties).indexedCosequence.value)
        Target.pure(extractedProps)
      }
      .orRefine { case x: Schema[_] if Option(x.get$ref()).isDefined => x }(comp =>
        Target.raiseUserError(s"Attempted to extractProperties for a ${comp.unwrapTracker.getClass()}, unsure what to do here (${comp.showHistory})")
      )
      .getOrElse(Target.pure(List.empty[(String, Tracker[Schema[_]])]))

  private def transformProperty(
      clsName: String,
      dtoPackage: List[String],
      supportPackage: List[String],
      concreteTypes: List[PropMeta[ScalaLanguage]]
  )(
      name: String,
      fieldName: String,
      property: Tracker[Schema[_]],
      meta: core.ResolvedType[ScalaLanguage],
      requirement: PropertyRequirement,
      isCustomType: Boolean,
      defaultValue: Option[scala.meta.Term]
  )(implicit Lt: LanguageTerms[ScalaLanguage, Target]): Target[ProtocolParameter[ScalaLanguage]] =
    Target.log.function(s"transformProperty") {
      val fallbackRawType = ReifiedRawType.of(property.downField("type", _.getType()).unwrapTracker, property.downField("format", _.getFormat()).unwrapTracker)
      for {
        _ <- Target.log.debug(s"Args: (${clsName}, ${name}, ...)")

        readOnlyKey = Option(name).filter(_ => property.downField("readOnly", _.getReadOnly()).unwrapTracker.contains(true))
        emptyToNull = property
          .refine { case d: DateSchema => d }(d => EmptyValueIsNull(d))
          .orRefine { case dt: DateTimeSchema => dt }(dt => EmptyValueIsNull(dt))
          .orRefine { case s: StringSchema => s }(s => EmptyValueIsNull(s))
          .toOption
          .flatten
          .getOrElse(EmptyIsEmpty)

        dataRedaction = DataRedaction(property).getOrElse(DataVisible)

        (tpe, classDep, rawType) <- meta match {
          case core.Resolved(declType, classDep, _, rawType @ LiteralRawType(Some(rawTypeStr), rawFormat)) if isFile(rawTypeStr, rawFormat) && !isCustomType =>
            // assume that binary data are represented as a string. allow users to override.
            Target.pure((t"String", classDep, rawType))
          case core.Resolved(declType, classDep, _, rawType) =>
            for {
              validatedType <- applyValidations(clsName, declType, property)
            } yield (validatedType, classDep, rawType)
          case core.Deferred(tpeName) =>
            val tpe = concreteTypes.find(_.clsName == tpeName).map(_.tpe).getOrElse {
              println(s"Unable to find definition for ${tpeName}, just inlining")
              Type.Name(tpeName)
            }
            for {
              validatedType <- applyValidations(clsName, tpe, property)
            } yield (validatedType, Option.empty, fallbackRawType)
          case core.DeferredArray(tpeName, containerTpe) =>
            val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
            val innerType    = concreteType.getOrElse(Type.Name(tpeName))
            val tpe          = t"${containerTpe.getOrElse(t"_root_.scala.Vector")}[$innerType]"
            for {
              validatedType <- applyValidations(clsName, tpe, property)
            } yield (validatedType, Option.empty, ReifiedRawType.ofVector(fallbackRawType))
          case core.DeferredMap(tpeName, customTpe) =>
            val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
            val innerType    = concreteType.getOrElse(Type.Name(tpeName))
            val tpe          = t"${customTpe.getOrElse(t"_root_.scala.Predef.Map")}[_root_.scala.Predef.String, $innerType]"
            for {
              validatedType <- applyValidations(clsName, tpe, property)
            } yield (validatedType, Option.empty, ReifiedRawType.ofMap(fallbackRawType))
        }
        fieldPattern: Tracker[Option[String]] = property.downField("pattern", _.getPattern)
        collectionElementPattern: Option[Tracker[String]] =
          property.downField("items", _.getItems).indexedDistribute.flatMap(_.downField("pattern", _.getPattern).indexedDistribute)

        pattern = collectionElementPattern.fold(fieldPattern.map(PropertyValidations))(
          _.map(regex => PropertyValidations(Some(regex)))
        )

        presence     <- Lt.selectTerm(NonEmptyList.ofInitLast(supportPackage, "Presence"))
        presenceType <- Lt.selectType(NonEmptyList.ofInitLast(supportPackage, "Presence"))
        (finalDeclType, finalDefaultValue) = requirement match {
          case PropertyRequirement.Required => tpe -> defaultValue
          case PropertyRequirement.Optional | PropertyRequirement.Configured(PropertyRequirement.Optional, PropertyRequirement.Optional) =>
            t"$presenceType[$tpe]" -> defaultValue.map(t => q"$presence.Present($t)").orElse(Some(q"$presence.Absent"))
          case _: PropertyRequirement.OptionalRequirement | _: PropertyRequirement.Configured =>
            t"Option[$tpe]" -> defaultValue.map(t => q"Option($t)").orElse(Some(q"None"))
          case PropertyRequirement.OptionalNullable =>
            t"$presenceType[Option[$tpe]]" -> defaultValue.map(t => q"$presence.Present($t)")
        }
        term = param"${Term.Name(fieldName)}: ${finalDeclType}".copy(default = finalDefaultValue)
        dep  = classDep.filterNot(_.value == clsName) // Filter out our own class name
      } yield ProtocolParameter[ScalaLanguage](
        term,
        tpe,
        RawParameterName(name),
        dep,
        rawType,
        readOnlyKey,
        emptyToNull,
        dataRedaction,
        requirement,
        finalDefaultValue,
        pattern
      )
    }

  private def renderDTOClass(
      clsName: String,
      supportPackage: List[String],
      selfParams: List[ProtocolParameter[ScalaLanguage]],
      parents: List[SuperClass[ScalaLanguage]] = Nil
  ) = {
    val discriminators     = parents.flatMap(_.discriminators)
    val discriminatorNames = discriminators.map(_.propertyName).toSet
    val parentOpt = if (parents.exists(s => s.discriminators.nonEmpty)) {
      parents.headOption
    } else {
      None
    }
    val params = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(param => discriminatorNames.contains(param.term.name.value))

    val terms = params.map(_.term)

    val toStringMethod = if (params.exists(_.dataRedaction != DataVisible)) {
      def mkToStringTerm(param: ProtocolParameter[ScalaLanguage]): Term = param match {
        case param if param.dataRedaction == DataVisible => q"${Term.Name(param.term.name.value)}.toString()"
        case _                                           => Lit.String("[redacted]")
      }

      val toStringTerms = params.map(p => List(mkToStringTerm(p))).intercalate(List(Lit.String(",")))

      List[Defn.Def](
        q"override def toString: String = ${toStringTerms.foldLeft[Term](Lit.String(s"${clsName}("))((accum, term) => q"$accum + $term")} + ${Lit.String(")")}"
      )
    } else {
      List.empty[Defn.Def]
    }

    val code = parentOpt
      .fold(q"""case class ${Type.Name(clsName)}(..${terms}) { ..$toStringMethod }""")(parent =>
        q"""case class ${Type.Name(clsName)}(..${terms}) extends ..${init"${Type.Name(parent.clsName)}(...$Nil)" :: parent.interfaces.map(a =>
            init"${Type.Name(a)}(...$Nil)"
          )} { ..$toStringMethod }"""
      )

    Target.pure(code)
  }

  private def encodeModel(
      clsName: String,
      dtoPackage: List[String],
      selfParams: List[ProtocolParameter[ScalaLanguage]],
      parents: List[SuperClass[ScalaLanguage]] = Nil
  ) = {
    val discriminators                = parents.flatMap(_.discriminators)
    val discriminatorNames            = discriminators.map(_.propertyName).toSet
    val allParams                     = parents.reverse.flatMap(_.params) ++ selfParams
    val (discriminatorParams, params) = allParams.partition(param => discriminatorNames.contains(param.name.value))
    val readOnlyKeys: List[String]    = params.flatMap(_.readOnlyKey).toList
    val typeName                      = Type.Name(clsName)
    val encVal = {
      def encodeStatic(param: ProtocolParameter[ScalaLanguage], clsName: String) =
        q"""(${Lit.String(param.name.value)}, _root_.io.circe.Json.fromString(${Lit.String(clsName)}))"""

      def encodeRequired(param: ProtocolParameter[ScalaLanguage]) =
        q"""(${Lit.String(param.name.value)}, a.${Term.Name(param.term.name.value)}.asJson)"""

      def encodeOptional(param: ProtocolParameter[ScalaLanguage]) = {
        val name = Lit.String(param.name.value)
        q"a.${Term.Name(param.term.name.value)}.fold(ifAbsent = None, ifPresent = value => Some($name -> value.asJson))"
      }

      val (optional, pairs): (List[Term.Apply], List[Term.Tuple]) = params.partitionEither { param =>
        val name = Lit.String(param.name.value)
        param.propertyRequirement match {
          case PropertyRequirement.Required | PropertyRequirement.RequiredNullable | PropertyRequirement.OptionalLegacy =>
            Right(encodeRequired(param))
          case PropertyRequirement.Optional | PropertyRequirement.OptionalNullable =>
            Left(encodeOptional(param))
          case PropertyRequirement.Configured(PropertyRequirement.Optional, PropertyRequirement.Optional) =>
            Left(encodeOptional(param))
          case PropertyRequirement.Configured(PropertyRequirement.RequiredNullable | PropertyRequirement.OptionalLegacy, _) =>
            Right(encodeRequired(param))
          case PropertyRequirement.Configured(PropertyRequirement.Optional, _) =>
            Left(q"""a.${Term.Name(param.term.name.value)}.map(value => (${Lit.String(param.name.value)}, value.asJson))""")
        }
      }

      val pairsWithStatic = pairs ++ discriminatorParams.map(encodeStatic(_, clsName))
      val simpleCase      = q"_root_.scala.Vector(..${pairsWithStatic})"
      val allFields = optional.foldLeft[Term](simpleCase) { (acc, field) =>
        q"$acc ++ $field"
      }

      q"""
              ${circeVersion.encoderObjectCompanion}.instance[${Type.Name(clsName)}](a => _root_.io.circe.JsonObject.fromIterable($allFields))
            """
    }
    val (readOnlyDefn, readOnlyFilter) = NonEmptyList.fromList(readOnlyKeys).fold((List.empty[Stat], identity[Term] _)) { roKeys =>
      (
        List(q"val readOnlyKeys = _root_.scala.Predef.Set[_root_.scala.Predef.String](..${roKeys.toList.map(Lit.String(_))})"),
        encVal => q"$encVal.mapJsonObject(_.filterKeys(key => !(readOnlyKeys contains key)))"
      )
    }

    Target.pure(Option(q"""
          implicit val ${suffixClsName("encode", clsName)}: ${circeVersion.encoderObject}[${Type.Name(clsName)}] = {
            ..${readOnlyDefn};
            ${readOnlyFilter(encVal)}
          }
        """))
  }

  private def decodeModel(
      clsName: String,
      dtoPackage: List[String],
      supportPackage: List[String],
      selfParams: List[ProtocolParameter[ScalaLanguage]],
      parents: List[SuperClass[ScalaLanguage]] = Nil
  )(implicit Lt: LanguageTerms[ScalaLanguage, Target]): Target[Option[Defn.Val]] = {
    val discriminators            = parents.flatMap(_.discriminators)
    val discriminatorNames        = discriminators.map(_.propertyName).toSet
    val allParams                 = parents.reverse.flatMap(_.params) ++ selfParams
    val params                    = allParams.filterNot(param => discriminatorNames.contains(param.name.value))
    val needsEmptyToNull: Boolean = params.exists(_.emptyToNull == EmptyIsNull)
    val paramCount                = params.length
    for {
      presence <- Lt.selectTerm(NonEmptyList.ofInitLast(supportPackage, "Presence"))
      decVal <-
        if (paramCount == 0) {
          Target.pure(
            q"""
                 new _root_.io.circe.Decoder[${Type.Name(clsName)}] {
                    final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[${Type.Name(clsName)}] =
                      _root_.scala.Right(${Term.Name(clsName)}())
                  }
                """
          )
        } else {
          params.zipWithIndex
            .traverse { case (param, idx) =>
              for {
                rawTpe <- Target.fromOption(param.term.decltpe, UserError("Missing type"))
                tpe <- rawTpe match {
                  case tpe: Type => Target.pure(tpe)
                  case x         => Target.raiseUserError(s"Unsure how to map ${x.structure}, please report this bug!")
                }
              } yield {
                val term = Term.Name(s"v$idx")
                val name = Lit.String(param.name.value)

                val emptyToNull: Term => Term = if (param.emptyToNull == EmptyIsNull) { t =>
                  q"$t.withFocus(j => j.asString.fold(j)(s => if(s.isEmpty) _root_.io.circe.Json.Null else j))"
                } else identity _

                val decodeField: Type => NonEmptyVector[Term => Term] = { tpe =>
                  NonEmptyVector.of[Term => Term](
                    t => q"$t.downField($name)",
                    emptyToNull,
                    t => q"$t.as[${tpe}]"
                  )
                }

                val decodeOptionalField: Type => (Term => Term, Term) => NonEmptyVector[Term => Term] = { tpe => (present, absent) =>
                  NonEmptyVector.of[Term => Term](t => q"""
                        ((c: _root_.io.circe.HCursor) =>
                          c
                            .value
                            .asObject
                            .filter(!_.contains($name))
                            .fold(${emptyToNull(q"c.downField($name)")}.as[${tpe}].map(x => ${present(q"x")})) { _ =>
                              _root_.scala.Right($absent)
                            }
                        )($t)
                      """)
                }

                def decodeOptionalRequirement(
                    param: ProtocolParameter[ScalaLanguage]
                ): PropertyRequirement.OptionalRequirement => NonEmptyVector[Term => Term] = {
                  case PropertyRequirement.OptionalLegacy =>
                    decodeField(tpe)
                  case PropertyRequirement.RequiredNullable =>
                    decodeField(t"_root_.io.circe.Json") :+ (t => q"$t.flatMap(_.as[${tpe}])")
                  case PropertyRequirement.Optional => // matched only where there is inconsistency between encoder and decoder
                    decodeOptionalField(param.baseType)(x => q"Option($x)", q"None")
                }

                val parseTermAccessors: NonEmptyVector[Term => Term] = param.propertyRequirement match {
                  case PropertyRequirement.Required =>
                    decodeField(tpe)
                  case PropertyRequirement.OptionalNullable =>
                    decodeOptionalField(t"Option[${param.baseType}]")(x => q"$presence.present($x)", q"$presence.absent")
                  case PropertyRequirement.Optional | PropertyRequirement.Configured(PropertyRequirement.Optional, PropertyRequirement.Optional) =>
                    decodeOptionalField(param.baseType)(x => q"$presence.present($x)", q"$presence.absent")
                  case requirement: PropertyRequirement.OptionalRequirement =>
                    decodeOptionalRequirement(param)(requirement)
                  case PropertyRequirement.Configured(_, decoderRequirement) =>
                    decodeOptionalRequirement(param)(decoderRequirement)
                }

                val parseTerm = parseTermAccessors.foldLeft[Term](q"c")((acc, next) => next(acc))
                val _enum     = enumerator"""${Pat.Var(term)} <- $parseTerm"""
                (term, _enum)
              }
            }
            .map { pairs =>
              val (terms, enumerators) = pairs.unzip
              q"""
                  new _root_.io.circe.Decoder[${Type.Name(clsName)}] {
                    final def apply(c: _root_.io.circe.HCursor): _root_.io.circe.Decoder.Result[${Type.Name(clsName)}] =
                      for {
                        ..${enumerators}
                      } yield ${Term.Name(clsName)}(..${terms})
                  }
                """
            }
        }
    } yield Option(q"""
            implicit val ${suffixClsName("decode", clsName)}: _root_.io.circe.Decoder[${Type.Name(clsName)}] = $decVal
          """)
  }

  private def renderDTOStaticDefns(
      clsName: String,
      deps: List[scala.meta.Term.Name],
      encoder: Option[scala.meta.Defn.Val],
      decoder: Option[scala.meta.Defn.Val],
      protocolParameters: List[ProtocolParameter[ScalaLanguage]]
  ) = {
    val extraImports: List[Import] = deps.map { term =>
      q"import ${term}._"
    }

    Target.pure(
      StaticDefns[ScalaLanguage](
        className = clsName,
        extraImports = extraImports,
        definitions = (encoder ++ decoder).toList
      )
    )
  }

  private def extractArrayType(arr: core.ResolvedType[ScalaLanguage], concreteTypes: List[PropMeta[ScalaLanguage]]) =
    for {
      result <- arr match {
        case core.Resolved(tpe, dep, default, _) => Target.pure(tpe)
        case core.Deferred(tpeName) =>
          Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), UserError(s"Unresolved reference ${tpeName}"))
        case core.DeferredArray(tpeName, containerTpe) =>
          Target.fromOption(
            lookupTypeName(tpeName, concreteTypes)(tpe => t"${containerTpe.getOrElse(t"_root_.scala.Vector")}[${tpe}]"),
            UserError(s"Unresolved reference ${tpeName}")
          )
        case core.DeferredMap(tpeName, customTpe) =>
          Target.fromOption(
            lookupTypeName(tpeName, concreteTypes)(tpe =>
              t"_root_.scala.Vector[${customTpe.getOrElse(t"_root_.scala.Predef.Map")}[_root_.scala.Predef.String, ${tpe}]]"
            ),
            UserError(s"Unresolved reference ${tpeName}")
          )
      }
    } yield result

  private def extractConcreteTypes(definitions: Either[String, List[PropMeta[ScalaLanguage]]]) =
    definitions.fold[Target[List[PropMeta[ScalaLanguage]]]](Target.raiseUserError _, Target.pure _)

  private def protocolImports() =
    Target.pure(
      List(
        q"import cats.syntax.either._",
        q"import io.circe.syntax._",
        q"import cats.instances.all._"
      )
    )

  override def staticProtocolImports(pkgName: List[String]): Target[List[Import]] = {
    val implicitsRef: Term.Ref = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
    Target.pure(
      List(
        q"import cats.implicits._",
        q"import cats.data.EitherT"
      ) :+ q"import $implicitsRef._"
    )
  }

  private def packageObjectImports() =
    Target.pure(List.empty)

  override def generateSupportDefinitions() = {
    val presenceTrait =
      q"""sealed trait Presence[+T] extends _root_.scala.Product with _root_.scala.Serializable {
              def fold[R](ifAbsent: => R,
                          ifPresent: T => R): R
              def map[R](f: T => R): Presence[R] = fold(Presence.absent, a => Presence.present(f(a)))

              def toOption: Option[T] = fold[Option[T]](None, Some(_))
            }
           """
    val presenceObject =
      q"""
            object Presence {
              def absent[R]: Presence[R] = Absent
              def present[R](value: R): Presence[R] = Present(value)
              case object Absent extends Presence[Nothing] {
                def fold[R](ifAbsent: => R,
                         ifValue: Nothing => R): R = ifAbsent
              }
              final case class Present[+T](value: T) extends Presence[T] {
                def fold[R](ifAbsent: => R,
                         ifPresent: T => R): R = ifPresent(value)
              }

              def fromOption[T](value: Option[T]): Presence[T] =
                value.fold[Presence[T]](Absent)(Present(_))

              implicit object PresenceFunctor extends cats.Functor[Presence] {
                def map[A, B](fa: Presence[A])(f: A => B): Presence[B] = fa.fold[Presence[B]](Presence.absent, a => Presence.present(f(a)))
              }
            }
           """
    val presenceDefinition = SupportDefinition[ScalaLanguage](q"Presence", Nil, List(presenceTrait, presenceObject), insideDefinitions = false)
    Target.pure(List(presenceDefinition))
  }

  private def packageObjectContents() =
    Target.pure(
      List(
        q"implicit val guardrailDecodeInstant: _root_.io.circe.Decoder[java.time.Instant] = _root_.io.circe.Decoder[java.time.Instant].or(_root_.io.circe.Decoder[_root_.scala.Long].map(java.time.Instant.ofEpochMilli))",
        q"implicit val guardrailDecodeLocalDate: _root_.io.circe.Decoder[java.time.LocalDate] = _root_.io.circe.Decoder[java.time.LocalDate].or(_root_.io.circe.Decoder[java.time.Instant].map(_.atZone(java.time.ZoneOffset.UTC).toLocalDate))",
        q"implicit val guardrailDecodeLocalDateTime: _root_.io.circe.Decoder[java.time.LocalDateTime] = _root_.io.circe.Decoder[java.time.LocalDateTime]",
        q"implicit val guardrailDecodeLocalTime: _root_.io.circe.Decoder[java.time.LocalTime] = _root_.io.circe.Decoder[java.time.LocalTime]",
        q"implicit val guardrailDecodeOffsetDateTime: _root_.io.circe.Decoder[java.time.OffsetDateTime] = _root_.io.circe.Decoder[java.time.OffsetDateTime].or(_root_.io.circe.Decoder[java.time.Instant].map(_.atZone(java.time.ZoneOffset.UTC).toOffsetDateTime))",
        q"implicit val guardrailDecodeZonedDateTime: _root_.io.circe.Decoder[java.time.ZonedDateTime] = _root_.io.circe.Decoder[java.time.ZonedDateTime]",
        q"implicit val guardrailDecodeBase64String: _root_.io.circe.Decoder[Base64String] = _root_.io.circe.Decoder[_root_.scala.Predef.String].emapTry(v => scala.util.Try(java.util.Base64.getDecoder.decode(v))).map(new Base64String(_))",
        q"implicit val guardrailEncodeInstant: _root_.io.circe.Encoder[java.time.Instant] = _root_.io.circe.Encoder[java.time.Instant]",
        q"implicit val guardrailEncodeLocalDate: _root_.io.circe.Encoder[java.time.LocalDate] = _root_.io.circe.Encoder[java.time.LocalDate]",
        q"implicit val guardrailEncodeLocalDateTime: _root_.io.circe.Encoder[java.time.LocalDateTime] = _root_.io.circe.Encoder[java.time.LocalDateTime]",
        q"implicit val guardrailEncodeLocalTime: _root_.io.circe.Encoder[java.time.LocalTime] = _root_.io.circe.Encoder[java.time.LocalTime]",
        q"implicit val guardrailEncodeOffsetDateTime: _root_.io.circe.Encoder[java.time.OffsetDateTime] = _root_.io.circe.Encoder[java.time.OffsetDateTime]",
        q"implicit val guardrailEncodeZonedDateTime: _root_.io.circe.Encoder[java.time.ZonedDateTime] = _root_.io.circe.Encoder[java.time.ZonedDateTime]",
        q"implicit val guardrailEncodeBase64String: _root_.io.circe.Encoder[Base64String] = _root_.io.circe.Encoder[_root_.scala.Predef.String].contramap[Base64String](v => new _root_.scala.Predef.String(java.util.Base64.getEncoder.encode(v.data)))"
      )
    )

  private def implicitsObject() = Target.pure(None)

  private def extractSuperClass(
      spec: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])]
  ) = {
    def allParents: Tracker[Schema[_]] => Target[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]] =
      _.refine[Target[List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])]]] { case x: ComposedSchema => x }(
        _.downField("allOf", _.getAllOf()).indexedDistribute.filter(_.downField("$ref", _.get$ref()).unwrapTracker.nonEmpty) match {
          case head :: tail =>
            definitions
              .collectFirst {
                case (clsName, e) if head.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/$clsName")) =>
                  val thisParent = (clsName, e, tail)
                  allParents(e).map(otherParents => thisParent :: otherParents)
              }
              .getOrElse(
                Target.raiseUserError(s"Reference ${head.downField("$ref", _.get$ref()).unwrapTracker} not found among definitions (${head.showHistory})")
              )
          case _ => Target.pure(List.empty)
        }
      ).getOrElse(Target.pure(List.empty))
    allParents(spec)
  }

  private def renderADTStaticDefns(
      clsName: String,
      discriminator: Discriminator[ScalaLanguage],
      encoder: Option[scala.meta.Defn.Val],
      decoder: Option[scala.meta.Defn.Val]
  ) =
    Target.pure(
      StaticDefns[ScalaLanguage](
        className = clsName,
        extraImports = List.empty[Import],
        definitions = List[Option[Defn]](
          Some(q"val discriminator: String = ${Lit.String(discriminator.propertyName)}"),
          encoder,
          decoder
        ).flatten
      )
    )

  private def decodeADT(clsName: String, discriminator: Discriminator[ScalaLanguage], children: List[String] = Nil) = {
    val (childrenCases, childrenDiscriminators) = children.map { child =>
      val discriminatorValue = discriminator.mapping
        .collectFirst { case (value, elem) if elem.name == child => value }
        .getOrElse(child)
      (
        p"case ${Lit.String(discriminatorValue)} => c.as[${Type.Name(child)}]",
        discriminatorValue
      )
    }.unzip
    val code =
      q"""implicit val decoder: _root_.io.circe.Decoder[${Type.Name(clsName)}] = _root_.io.circe.Decoder.instance({ c =>
               val discriminatorCursor = c.downField(discriminator)
               discriminatorCursor.as[String].flatMap {
                 ..case $childrenCases;
                 case tpe =>
                   _root_.scala.Left(_root_.io.circe.DecodingFailure("Unknown value " ++ tpe ++ ${Lit
          .String(s" (valid: ${childrenDiscriminators.mkString(", ")})")}, discriminatorCursor.history))
               }
          })"""
    Target.pure(Some(code))
  }

  private def encodeADT(clsName: String, discriminator: Discriminator[ScalaLanguage], children: List[String] = Nil) = {
    val childrenCases = children.map { child =>
      val discriminatorValue = discriminator.mapping
        .collectFirst { case (value, elem) if elem.name == child => value }
        .getOrElse(child)
      p"case e:${Type.Name(child)} => e.asJsonObject.add(discriminator, _root_.io.circe.Json.fromString(${Lit.String(discriminatorValue)})).asJson"
    }
    val code =
      q"""implicit val encoder: _root_.io.circe.Encoder[${Type.Name(clsName)}] = _root_.io.circe.Encoder.instance {
            ..case $childrenCases
        }"""
    Target.pure(Some(code))
  }

  private def renderSealedTrait(
      className: String,
      params: List[ProtocolParameter[ScalaLanguage]],
      discriminator: Discriminator[ScalaLanguage],
      parents: List[SuperClass[ScalaLanguage]] = Nil,
      children: List[String] = Nil
  ) =
    for {
      testTerms <-
        params
          .map(_.term)
          .filter(_.name.value != discriminator.propertyName)
          .traverse { t =>
            for {
              tpe <- Target.fromOption(
                t.decltpe
                  .flatMap {
                    case tpe: Type => Some(tpe)
                    case x         => None
                  },
                UserError(t.decltpe.fold("Nothing to map")(x => s"Unsure how to map ${x.structure}, please report this bug!"))
              )
            } yield q"""def ${Term.Name(t.name.value)}: ${tpe}"""
          }
    } yield parents.headOption
      .fold(q"""trait ${Type.Name(className)} {..${testTerms}}""")(parent =>
        q"""trait ${Type.Name(className)} extends ${init"${Type.Name(parent.clsName)}(...$Nil)"} { ..${testTerms} } """
      )
}
