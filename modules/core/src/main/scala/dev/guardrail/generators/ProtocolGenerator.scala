package dev.guardrail.generators

import _root_.io.swagger.v3.oas.models._
import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, _ }
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._
import dev.guardrail._
import dev.guardrail.core.{ DataRedacted, DataVisible, EmptyIsEmpty, EmptyIsNull, Mappish, Tracker }
import dev.guardrail.core.implicits._
import dev.guardrail.languages.LA
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.{
  CollectionsLibTerms,
  EnumSchema,
  HeldEnum,
  IntHeldEnum,
  LanguageTerms,
  LongHeldEnum,
  NumberEnumSchema,
  ObjectEnumSchema,
  ProtocolTerms,
  RenderedIntEnum,
  RenderedLongEnum,
  RenderedStringEnum,
  StringEnumSchema,
  StringHeldEnum,
  SwaggerTerms
}
import cats.Foldable
import dev.guardrail.core.extract.Default
import scala.jdk.CollectionConverters._

case class ProtocolDefinitions[L <: LA](
    elems: List[StrictProtocolElems[L]],
    protocolImports: List[L#Import],
    packageObjectImports: List[L#Import],
    packageObjectContents: List[L#Statement],
    implicitsObject: Option[(L#TermName, L#ObjectDefinition)]
)

object ProtocolGenerator {
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

  type WrapEnumSchema[A] = Schema[A] => EnumSchema
  implicit val wrapNumberEnumSchema: WrapEnumSchema[Number] = NumberEnumSchema.apply _
  implicit val wrapObjectEnumSchema: WrapEnumSchema[Object] = ObjectEnumSchema.apply _
  implicit val wrapStringEnumSchema: WrapEnumSchema[String] = StringEnumSchema.apply _

  private[this] def fromEnum[L <: LA, F[_], A](
      clsName: String,
      schema: Tracker[Schema[A]],
      dtoPackage: List[String],
      components: Tracker[Option[Components]]
  )(implicit
      P: ProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F],
      wrapEnumSchema: WrapEnumSchema[A]
  ): F[Either[String, EnumDefinition[L]]] = {
    import P._
    import Sc._
    import Sw._

    def validProg(held: HeldEnum, tpe: L#Type, fullType: L#Type): F[EnumDefinition[L]] =
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
              wrappedValues = RenderedStringEnum(elems)
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
              wrappedValues = RenderedIntEnum(elems)
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
              wrappedValues = RenderedLongEnum(elems)
            } yield (pascalValues, wrappedValues)
        }
        members <- renderMembers(clsName, wrappedValues)
        encoder <- encodeEnum(clsName, tpe)
        decoder <- decodeEnum(clsName, tpe)

        defn        <- renderClass(clsName, tpe, wrappedValues)
        staticDefns <- renderStaticDefns(clsName, tpe, members, pascalValues, encoder, decoder)
        classType   <- pureTypeName(clsName)
      } yield EnumDefinition[L](clsName, classType, fullType, wrappedValues, defn, staticDefns)

    for {
      enum          <- extractEnum(schema.map(wrapEnumSchema))
      customTpeName <- SwaggerUtil.customTypeName(schema)
      (tpe, _)      <- SwaggerUtil.determineTypeName(schema, Tracker.cloneHistory(schema, customTpeName), components)
      fullType      <- selectType(NonEmptyList.ofInitLast(dtoPackage, clsName))
      res           <- enum.traverse(validProg(_, tpe, fullType))
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
  private[this] def fromPoly[L <: LA, F[_]](
      hierarchy: ClassParent[L],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[L, F],
      P: ProtocolTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[ProtocolElems[L]] = {
    import P._
    import Sc._

    def child(hierarchy: ClassHierarchy[L]): List[String] =
      hierarchy.children.map(_.name) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy[L]): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.name :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator

    for {
      parents <- hierarchy.model
        .refine[F[List[SuperClass[L]]]] { case c: ComposedSchema => c }(
          extractParents(_, definitions, concreteTypes, dtoPackage, supportPackage, defaultPropertyRequirement, components)
        )
        .getOrElse(List.empty[SuperClass[L]].pure[F])
      props <- extractProperties(hierarchy.model)
      requiredFields = hierarchy.required ::: hierarchy.children.flatMap(_.required)
      params <- props.traverse { case (name, prop) =>
        for {
          typeName <- formatTypeName(name).map(formattedName => NonEmptyList.of(hierarchy.name, formattedName))
          propertyRequirement = getPropertyRequirement(prop, requiredFields.contains(name), defaultPropertyRequirement)
          customType <- SwaggerUtil.customTypeName(prop)
          resolvedType <- SwaggerUtil
            .propMeta[L, F](
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
            customType.isDefined,
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
    } yield ADT[L](
      name = hierarchy.name,
      tpe = tpe,
      fullType = fullType,
      trt = definition,
      staticDefns = staticDefns
    )
  }

  private def extractParents[L <: LA, F[_]](
      elem: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])],
      concreteTypes: List[PropMeta[L]],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[L, F],
      P: ProtocolTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[List[SuperClass[L]]] = {
    import P._
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
            _.refine[F[List[Discriminator[L]]]] { case m: ObjectSchema => m }(m => Discriminator.fromSchema(m).map(_.toList))
              .getOrElse(List.empty[Discriminator[L]].pure[F])
          )
        } yield tpe
          .map(
            SuperClass[L](
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

  private[this] def fromModel[L <: LA, F[_]](
      clsName: NonEmptyList[String],
      model: Tracker[Schema[_]],
      parents: List[SuperClass[L]],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[L, F],
      P: ProtocolTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[Either[String, ClassDefinition[L]]] = {
    import P._
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
        case classDefinition: ClassDefinition[L] =>
          for {
            widenClass          <- widenClassDefinition(classDefinition.cls)
            companionTerm       <- pureTermName(classDefinition.name)
            companionDefinition <- wrapToObject(companionTerm, classDefinition.staticDefns.extraImports, classDefinition.staticDefns.definitions)
            widenCompanion      <- companionDefinition.traverse(widenObjectDefinition)
          } yield List(widenClass) ++ widenCompanion.fold(classDefinition.staticDefns.definitions)(List(_))
        case enumDefinition: EnumDefinition[L] =>
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
      if (parents.isEmpty && props.isEmpty) Left("Entity isn't model"): Either[String, ClassDefinition[L]]
      else tpe.toRight("Empty entity name").map(ClassDefinition[L](clsName.last, _, fullType, defn, finalStaticDefns, parents))
    }

  }

  private def prepareProperties[L <: LA, F[_]](
      clsName: NonEmptyList[String],
      propertyToTypeLookup: Map[String, String],
      props: List[(String, Tracker[Schema[_]])],
      requiredFields: List[String],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[L, F],
      P: ProtocolTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[(List[ProtocolParameter[L]], List[NestedProtocolElems[L]])] = {
    import P._
    import Sc._
    def getClsName(name: String): NonEmptyList[String] = propertyToTypeLookup.get(name).map(NonEmptyList.of(_)).getOrElse(clsName)

    def processProperty(name: String, schema: Tracker[Schema[_]]): F[Option[Either[String, NestedProtocolElems[L]]]] =
      for {
        nestedClassName <- formatTypeName(name).map(formattedName => getClsName(name).append(formattedName))
        defn <- schema
          .refine[F[Option[Either[String, NestedProtocolElems[L]]]]] { case x: ObjectSchema => x }(o =>
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
          .orRefine { case m: MapSchema => m } { m =>
            // TODO: #1591: What we are emitting here is the inner class `Z` in the expression:
            //                  `foo: Option[Map[String, Z]]`
            //              Unfortunately, that means that if we wanted to properly parse and incorporate
            //              those fields, we would need to do something more creative like
            //                  `case class Bar(prop: A, prop2: B, extra: Map[String, Z])`
            //              so we could do
            //                  `foo: Option[Bar]`
            //              we would need an extra className for `Bar` (instead of just using nestedClassName),
            //              as well as to be more choosy about which fields we try to decode with `.as[Z]`,
            //              since we should intentionally and explicitly skip fields for which we have known
            //              mappings for, using the `additionalProperties` mapping only for fields we've never seen.
            NonEmptyList.fromList(m.downField("properties", _.getProperties).indexedDistribute.toList).foreach { props =>
              println(s"""WARNING: Unsupported mixing of properties and additionalProperties.
                         |         See https://github.com/guardrail-dev/guardrail/issues/1591
                         |         Ignored values: ${props.toList.map(_.showHistory).mkString(", ")}
                         |""".stripMargin)
            }
            m.downField("additionalProperties", _.getAdditionalProperties)
              .indexedDistribute
              .flatTraverse(
                _.refine { case x: ObjectSchema => x }(obj =>
                  fromModel(nestedClassName, obj, List.empty, concreteTypes, definitions, dtoPackage, supportPackage, defaultPropertyRequirement, components)
                ).toOption.sequence
              )
              .widen
          }
          .getOrElse(Option.empty[Either[String, NestedProtocolElems[L]]].pure[F])
      } yield defn

    for {
      paramsAndNestedDefinitions <- props.traverse[F, (Tracker[ProtocolParameter[L]], Option[NestedProtocolElems[L]])] { case (name, schema) =>
        for {
          typeName              <- formatTypeName(name).map(formattedName => getClsName(name).append(formattedName))
          tpe                   <- selectType(typeName)
          maybeNestedDefinition <- processProperty(name, schema)
          resolvedType          <- SwaggerUtil.propMetaWithName(tpe, schema, components)
          customType            <- SwaggerUtil.customTypeName(schema)
          propertyRequirement = getPropertyRequirement(schema, requiredFields.contains(name), defaultPropertyRequirement)
          defValue  <- defaultValue(typeName, schema, propertyRequirement, definitions)
          fieldName <- formatFieldName(name)
          parameter <- transformProperty(getClsName(name).last, dtoPackage, supportPackage, concreteTypes)(
            name,
            fieldName,
            schema,
            resolvedType,
            propertyRequirement,
            customType.isDefined,
            defValue
          )
        } yield (Tracker.cloneHistory(schema, parameter), maybeNestedDefinition.flatMap(_.toOption))
      }
      (params, nestedDefinitions) = paramsAndNestedDefinitions.unzip
      deduplicatedParams <- deduplicateParams(params)
      unconflictedParams <- fixConflictingNames(deduplicatedParams)
    } yield (unconflictedParams, nestedDefinitions.flatten)
  }

  private def deduplicateParams[L <: LA, F[_]](
      params: List[Tracker[ProtocolParameter[L]]]
  )(implicit Sw: SwaggerTerms[L, F], Sc: LanguageTerms[L, F]): F[List[ProtocolParameter[L]]] = {
    import Sc._
    Foldable[List]
      .foldLeftM[F, Tracker[ProtocolParameter[L]], List[ProtocolParameter[L]]](params, List.empty[ProtocolParameter[L]]) { (s, ta) =>
        val a = ta.unwrapTracker
        s.find(p => p.name == a.name) match {
          case None => (a :: s).pure[F]
          case Some(duplicate) =>
            for {
              newDefaultValue <- findCommonDefaultValue(ta.showHistory, a.defaultValue, duplicate.defaultValue)
              newRawType      <- findCommonRawType(ta.showHistory, a.rawType, duplicate.rawType)
            } yield {
              val emptyToNull        = if (Set(a.emptyToNull, duplicate.emptyToNull).contains(EmptyIsNull)) EmptyIsNull else EmptyIsEmpty
              val redactionBehaviour = if (Set(a.dataRedaction, duplicate.dataRedaction).contains(DataRedacted)) DataRedacted else DataVisible
              val mergedParameter = ProtocolParameter[L](
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

  private def fixConflictingNames[L <: LA, F[_]](params: List[ProtocolParameter[L]])(implicit Lt: LanguageTerms[L, F]): F[List[ProtocolParameter[L]]] = {
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
          param.pure[F]
        }
      }
    } yield newParams
  }

  def modelTypeAlias[L <: LA, F[_]](clsName: String, abstractModel: Tracker[Schema[_]], components: Tracker[Option[Components]])(implicit
      Fw: FrameworkTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[ProtocolElems[L]] = {
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
      tpe <- model.fold[F[L#Type]](objectType(None)) { m =>
        for {
          tpeName       <- SwaggerUtil.customTypeName[L, F, Tracker[ObjectSchema]](m)
          (declType, _) <- SwaggerUtil.determineTypeName[L, F](m, Tracker.cloneHistory(m, tpeName), components)
        } yield declType
      }
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def plainTypeAlias[L <: LA, F[_]](
      clsName: String
  )(implicit Fw: FrameworkTerms[L, F], Sc: LanguageTerms[L, F]): F[ProtocolElems[L]] = {
    import Fw._
    for {
      tpe <- objectType(None)
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def typeAlias[L <: LA, F[_]: Monad](clsName: String, tpe: L#Type): F[ProtocolElems[L]] =
    (RandomType[L](clsName, tpe): ProtocolElems[L]).pure[F]

  def fromArray[L <: LA, F[_]](clsName: String, arr: Tracker[ArraySchema], concreteTypes: List[PropMeta[L]], components: Tracker[Option[Components]])(implicit
      F: FrameworkTerms[L, F],
      P: ProtocolTerms[L, F],
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): F[ProtocolElems[L]] = {
    import P._
    for {
      deferredTpe <- SwaggerUtil.modelMetaType(arr, components)
      tpe         <- extractArrayType(deferredTpe, concreteTypes)
      ret         <- typeAlias[L, F](clsName, tpe)
    } yield ret
  }

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

  /** returns objects grouped into hierarchies
    */
  def groupHierarchies[L <: LA, F[_]](
      definitions: Mappish[List, String, Tracker[Schema[_]]]
  )(implicit Sc: LanguageTerms[L, F], Sw: SwaggerTerms[L, F]): F[(List[ClassParent[L]], List[(String, Tracker[Schema[_]])])] = {

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

    def children(cls: String): List[ClassChild[L]] = definitions.value.flatMap { case (clsName, comp) =>
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

    def classHierarchy(cls: String, model: Tracker[Schema[_]]): F[Option[ClassParent[L]]] =
      model
        .refine { case c: ComposedSchema => c }(c =>
          firstInHierarchy(c)
            .fold(Option.empty[Discriminator[L]].pure[F])(Discriminator.fromSchema[L, F])
            .map(_.map((_, getRequiredFieldsRec(c))))
        )
        .orRefine { case x: Schema[_] => x }(m => Discriminator.fromSchema(m).map(_.map((_, getRequiredFieldsRec(m)))))
        .getOrElse(Option.empty[(Discriminator[L], List[String])].pure[F])
        .map(_.map { case (discriminator, reqFields) => ClassParent(cls, model, children(cls), discriminator, reqFields) })

    Sw.log.function("groupHierarchies")(
      definitions.value
        .traverse { case (cls, model) =>
          for {
            hierarchy <- classHierarchy(cls, model)
          } yield hierarchy.filterNot(_.children.isEmpty).toLeft((cls, model))
        }
        .map(_.partitionEither[ClassParent[L], (String, Tracker[Schema[_]])](identity))
    )
  }

  def fromSwagger[L <: LA, F[_]](
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
  ): F[ProtocolDefinitions[L]] = {
    import P._
    import Sc._

    val components  = swagger.downField("components", _.getComponents())
    val definitions = components.flatDownField("schemas", _.getSchemas()).indexedCosequence
    Sw.log.function("ProtocolGenerator.fromSwagger")(for {
      (hierarchies, definitionsWithoutPoly) <- groupHierarchies(definitions)

      concreteTypes <- SwaggerUtil.extractConcreteTypes[L, F](definitions.value, components)
      polyADTs <- hierarchies.traverse(fromPoly(_, concreteTypes, definitions.value, dtoPackage, supportPackage.toList, defaultPropertyRequirement, components))
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
              enum             <- fromEnum(formattedClsName, m, dtoPackage, components)
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
              customTypeName <- SwaggerUtil.customTypeName(x)
              (declType, _)  <- SwaggerUtil.determineTypeName[L, F](x, Tracker.cloneHistory(x, customTypeName), components)
              alias          <- typeAlias[L, F](formattedClsName, declType)
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
              customTypeName <- SwaggerUtil.customTypeName(x)
              (declType, _)  <- SwaggerUtil.determineTypeName[L, F](x, Tracker.cloneHistory(x, customTypeName), components)
              alias          <- typeAlias[L, F](formattedClsName, declType)
            } yield enum.orElse(model).getOrElse(alias)
          )
          .valueOr(x =>
            for {
              formattedClsName <- formatTypeName(clsName)
              customTypeName   <- SwaggerUtil.customTypeName(x)
              (declType, _)    <- SwaggerUtil.determineTypeName[L, F](x, Tracker.cloneHistory(x, customTypeName), components)
              res              <- typeAlias[L, F](formattedClsName, declType)
            } yield res
          )
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()
      implicitsObject   <- implicitsObject()

      polyADTElems <- ProtocolElems.resolve[L, F](polyADTs)
      strictElems  <- ProtocolElems.resolve[L, F](elems)
    } yield ProtocolDefinitions[L](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents, implicitsObject))
  }

  private def defaultValue[L <: LA, F[_]](
      name: NonEmptyList[String],
      schema: Tracker[Schema[_]],
      requirement: PropertyRequirement,
      definitions: List[(String, Tracker[Schema[_]])]
  )(implicit
      Sc: LanguageTerms[L, F],
      Cl: CollectionsLibTerms[L, F]
  ): F[Option[L#Term]] = {
    import Sc._
    import Cl._
    val empty = Option.empty[L#Term].pure[F]
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
              customTpe <- SwaggerUtil.customMapTypeName(map)
              result    <- customTpe.fold(emptyMap().map(Option(_)))(_ => empty)
            } yield result
          )
          .orRefine { case arr: ArraySchema if requirement == PropertyRequirement.Required || requirement == PropertyRequirement.RequiredNullable => arr }(
            arr =>
              for {
                customTpe <- SwaggerUtil.customArrayTypeName(arr)
                result    <- customTpe.fold(emptyArray().map(Option(_)))(_ => empty)
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
}
