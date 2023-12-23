package dev.guardrail.generators.scala.jackson

import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, _ }
import _root_.io.swagger.v3.oas.models.{ Components, OpenAPI }
import cats.Foldable
import cats.data.NonEmptyList
import cats.syntax.all._
import scala.jdk.CollectionConverters._
import scala.meta._
import scala.reflect.runtime.universe.typeTag

import dev.guardrail.core
import dev.guardrail.core.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, DataRedaction, Default, EmptyValueIsNull }
import dev.guardrail.core.{ DataRedacted, DataVisible, EmptyIsEmpty, EmptyIsNull, LiteralRawType, Mappish, ReifiedRawType, SupportDefinition, Tracker }
import dev.guardrail.core.resolvers.ModelResolver
import dev.guardrail.generators.ProtocolGenerator.{ WrapEnumSchema, wrapNumberEnumSchema, wrapObjectEnumSchema, wrapStringEnumSchema }
import dev.guardrail.generators.protocol.{ ClassChild, ClassHierarchy, ClassParent }
import dev.guardrail.generators.scala.{ JacksonModelGenerator, ScalaLanguage }
import dev.guardrail.generators.spi.{ ModuleLoadResult, ProtocolGeneratorLoader }
import dev.guardrail.generators.{ ProtocolDefinitions, RawParameterName }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.PropertyRequirement.{ Optional, RequiredNullable }
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.protocol.{ Discriminator, PropertyRequirement }
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
import dev.guardrail.{ RuntimeFailure, Target, UserError }

class JacksonProtocolGeneratorLoader extends ProtocolGeneratorLoader {
  type L = ScalaLanguage
  def reified = typeTag[Target[ScalaLanguage]]
  // We do not support different versions of Jackson at this time, so if
  // this is desirable in the future we can adopt a similar strategy as is
  // used in CirceProtocolGenerator.
  val apply = ModuleLoadResult.forProduct1(ProtocolGeneratorLoader.label -> Seq(JacksonModelGenerator.mapping))(_ => JacksonProtocolGenerator.apply)
}

object JacksonProtocolGenerator {
  def apply: ProtocolTerms[ScalaLanguage, Target] =
    new JacksonProtocolGenerator
}

class JacksonProtocolGenerator private extends ProtocolTerms[ScalaLanguage, Target] {
  // NB: In OpenAPI 3.1 ObjectSchema was broadly replaced with JsonSchema.
  // This broke a lot of assumptions, but seems to indicate that we're moving
  // into a world where OAI has encoding information pushed to all models,
  // instead of hoping that the operation code generators or global
  // object-mappers can manage it all.
  //
  // This seems like a good change, but I'm opting to defer major refactors
  // until the particulars of this change have had a time to sink in.
  //
  // This extractor is copy/pasted to a few different classes in guardrail.
  // Should you copy it further, please copy this note as well.
  private object ObjectExtractor {
    def unapply(m: Schema[_]): Option[Schema[Object]] = m match {
      case m: ObjectSchema => Some(m)
      case m: JsonSchema   => Some(m)
      case _               => None
    }
  }

  private def discriminatorValue(discriminator: Discriminator[ScalaLanguage], className: String): String =
    discriminator.mapping
      .collectFirst { case (value, elem) if elem.name == className => value }
      .getOrElse(className)

  private val jsonIgnoreProperties = mod"""@com.fasterxml.jackson.annotation.JsonIgnoreProperties(ignoreUnknown = true)"""

  private def paramAnnotations(
      param: ProtocolParameter[ScalaLanguage],
      presenceSerType: Type,
      presenceDeserType: Type,
      optionNonNullDeserType: Type,
      optionNonMissingDeserType: Type,
      emptyIsNullDeserType: Type,
      emptyIsNullOptionDeserType: Type
  ): List[Mod] = {
    val jsonProperty = List(mod"@com.fasterxml.jackson.annotation.JsonProperty(${Lit.String(param.name.value)})")
    val containerDeserializers = param.term.decltpe match {
      case Some(t"Vector[$inner]")        => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
      case Some(t"List[$inner]")          => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
      case Some(t"IndexedSeq[$inner]")    => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
      case Some(t"Seq[$inner]")           => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
      case Some(t"Iterable[$inner]")      => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
      case Some(t"Map[$keyType, $inner]") => List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
      case _                              => List.empty
    }
    val presenceHandling = param.term.decltpe match {
      case Some(t"Presence[_]") | Some(Type.Apply.After_4_6_0(Type.Select(_, Type.Name("Presence")), _)) =>
        List(
          mod"@com.fasterxml.jackson.annotation.JsonInclude(value = com.fasterxml.jackson.annotation.JsonInclude.Include.NON_EMPTY)",
          mod"@com.fasterxml.jackson.databind.annotation.JsonSerialize(using = classOf[$presenceSerType])",
          mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$presenceDeserType])"
        )
      case _ => List.empty
    }
    val optionAndEmptyHandling = param.term.decltpe match {
      case Some(t"Option[$inner]") =>
        val encodeRequirement = param.propertyRequirement match {
          case PropertyRequirement.Configured(encoder, _) => encoder
          case other                                      => other
        }
        val decodeRequirement = param.propertyRequirement match {
          case PropertyRequirement.Configured(_, decoder) => decoder
          case other                                      => other
        }

        val serializers = encodeRequirement match {
          case Optional =>
            // When serializing, either include the non-null value or drop the property entirely
            List(mod"@com.fasterxml.jackson.annotation.JsonInclude(value = com.fasterxml.jackson.annotation.JsonInclude.Include.NON_EMPTY)")
          case _ =>
            // RequiredNullable: Always include the property, null if appropriate
            // OptionalLegacy: Legacy behavior is to serialize non-required, unset-nullability as null if not present
            List(mod"@com.fasterxml.jackson.annotation.JsonInclude(value = com.fasterxml.jackson.annotation.JsonInclude.Include.ALWAYS)")
        }

        val deserializers = decodeRequirement match {
          case Optional =>
            // When deserializing either a non-null value must be present, or no value at all
            List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$optionNonNullDeserType], contentAs = classOf[$inner])")
          case RequiredNullable =>
            // When deserializing, the property must be present, but can be null
            List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$optionNonMissingDeserType], contentAs = classOf[$inner])")
          case _ =>
            (param.emptyToNull, inner) match {
              case (EmptyIsNull, t"String") =>
                // OptionalLegacy: Same as above, but deserializes the empty string as None
                List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$emptyIsNullOptionDeserType])")
              case _ =>
                // OptionalLegacy: When deserializing the property can be present (null or non-null) or not present
                List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(contentAs = classOf[$inner])")
            }
        }

        serializers ++ deserializers

      case Some(t"String") if param.emptyToNull == EmptyIsNull =>
        // Causes a deserialization failure if the value is the empty string
        List(mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using = classOf[$emptyIsNullDeserType])")

      case _ =>
        List.empty
    }
    val notNull = List(mod"@constraints.NotNull")
    jsonProperty ++ containerDeserializers ++ presenceHandling ++ optionAndEmptyHandling ++ notNull
  }

  private def fixDefaultValue(param: ProtocolParameter[ScalaLanguage]): Option[Term] =
    param.propertyRequirement match {
      case Optional | RequiredNullable | PropertyRequirement.Configured(_, Optional | RequiredNullable) =>
        // We can't have a default value with Jackson, because it will allow us to deserialize
        // things that are not valid (nulls in the Optional case, missing property in the
        // RequiredNullable case). This is unfortunate because it makes the API of the case class
        // less ergonomic, but perhaps we can add an apply() method to the companion object to fix that up.
        None
      case _ => param.term.default
    }

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
          .orRefine { case ObjectExtractor(o) => o }(m =>
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
              prefixes      <- vendorPrefixes()
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
              prefixes      <- vendorPrefixes()
              (declType, _) <- ModelResolver.determineTypeName[ScalaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
              alias         <- typeAlias(formattedClsName, declType)
            } yield enum.orElse(model).getOrElse(alias)
          )
          .valueOr(x =>
            for {
              formattedClsName <- formatTypeName(clsName)
              prefixes         <- vendorPrefixes()
              (declType, _)    <- ModelResolver.determineTypeName[ScalaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
              res              <- typeAlias(formattedClsName, declType)
            } yield res
          )
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()
      implicitsObject   <- implicitsObject()
    } yield ProtocolDefinitions[ScalaLanguage](elems ++ polyADTs, protoImports, pkgImports, pkgObjectContents, implicitsObject))
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
  ): Target[StrictProtocolElems[ScalaLanguage]] = {
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
      params <- props.traverse { case (name, prop) =>
        for {
          typeName <- formatTypeName(name).map(formattedName => NonEmptyList.of(hierarchy.name, formattedName))
          propertyRequirement = getPropertyRequirement(prop, requiredFields.contains(name), defaultPropertyRequirement)
          prefixes <- vendorPrefixes()
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
            _.refine[Target[List[Discriminator[ScalaLanguage]]]] { case ObjectExtractor(m) => m }(m =>
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
          .refine[Target[Option[Either[String, NestedProtocolElems[ScalaLanguage]]]]] { case ObjectExtractor(x) => x }(o =>
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
      paramsAndNestedDefinitions <- props.traverse[Target, (Tracker[ProtocolParameter[ScalaLanguage]], Option[NestedProtocolElems[ScalaLanguage]])] {
        case (name, schema) =>
          for {
            typeName              <- formatTypeName(name).map(formattedName => getClsName(name).append(formattedName))
            tpe                   <- selectType(typeName)
            maybeNestedDefinition <- processProperty(name, schema)
            resolvedType          <- ModelResolver.propMetaWithName[ScalaLanguage, Target](Target.pure(tpe), schema, components)
            prefixes              <- vendorPrefixes()
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
  ): Target[StrictProtocolElems[ScalaLanguage]] = {
    import Cl._
    import Fw._
    val model: Option[Tracker[Schema[Object]]] = abstractModel
      .refine[Option[Tracker[Schema[Object]]]] { case ObjectExtractor(m) => m }(x => Option(x))
      .orRefine { case m: ComposedSchema => m }(
        _.downField("allOf", _.getAllOf()).indexedCosequence
          .get(1)
          .flatMap(
            _.refine { case ObjectExtractor(o) => o }(Option.apply)
              .orRefineFallback(_ => None)
          )
      )
      .orRefineFallback(_ => None)
    for {
      prefixes <- vendorPrefixes()
      tpe <- model.fold[Target[scala.meta.Type]](objectType(None)) { m =>
        for {
          (declType, _) <- ModelResolver.determineTypeName[ScalaLanguage, Target](m, Tracker.cloneHistory(m, CustomTypeName(m, prefixes)), components)
        } yield declType
      }
      res <- typeAlias(clsName, tpe)
    } yield res
  }

  private def plainTypeAlias(
      clsName: String
  )(implicit Fw: FrameworkTerms[ScalaLanguage, Target], Sc: LanguageTerms[ScalaLanguage, Target]): Target[StrictProtocolElems[ScalaLanguage]] = {
    import Fw._
    for {
      tpe <- objectType(None)
      res <- typeAlias(clsName, tpe)
    } yield res
  }

  private def typeAlias(clsName: String, tpe: scala.meta.Type): Target[StrictProtocolElems[ScalaLanguage]] =
    (RandomType[ScalaLanguage](clsName, tpe): StrictProtocolElems[ScalaLanguage]).pure[Target]

  private def fromArray(clsName: String, arr: Tracker[ArraySchema], concreteTypes: List[PropMeta[ScalaLanguage]], components: Tracker[Option[Components]])(
      implicit
      F: FrameworkTerms[ScalaLanguage, Target],
      P: ProtocolTerms[ScalaLanguage, Target],
      Sc: LanguageTerms[ScalaLanguage, Target],
      Cl: CollectionsLibTerms[ScalaLanguage, Target],
      Sw: OpenAPITerms[ScalaLanguage, Target]
  ): Target[StrictProtocolElems[ScalaLanguage]] =
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

    def firstInHierarchy(model: Tracker[Schema[_]]): Option[Tracker[Schema[Object]]] =
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
                .orRefine { case ObjectExtractor(o) => o }(x => Option(x))
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

  private def encodeEnum(className: String, tpe: Type): Target[Option[Defn]] =
    for {
      writeMethod <- tpe match {
        case t"Int"    => Target.pure(q"writeNumber")
        case t"Long"   => Target.pure(q"writeNumber")
        case t"String" => Target.pure(q"writeString")
        case other     => Target.raiseException(s"Unexpected type during enumeration encoder: ${className} was ${other}")
      }
    } yield Some(
      q"""
         class ${Type.Name(className + "Serializer")} extends com.fasterxml.jackson.databind.JsonSerializer[${Type.Name(className)}] {
           override def serialize(value: ${Type
          .Name(
            className
          )}, gen: com.fasterxml.jackson.core.JsonGenerator, serializers: com.fasterxml.jackson.databind.SerializerProvider): Unit = gen.${writeMethod}(value.value)
         }
       """
    )

  private def decodeEnum(className: String, tpe: Type): Target[Option[Defn]] =
    for {
      getter <- tpe match {
        case t"String" => Target.pure(q"getText")
        case t"Int"    => Target.pure(q"getIntValue")
        case t"Long"   => Target.pure(q"getLongValue")
        case other     => Target.raiseException(s"Unexpected type during enumeration decoder: ${className} was ${other}")
      }
    } yield Some(
      q"""
         class ${Type.Name(className + "Deserializer")} extends com.fasterxml.jackson.databind.JsonDeserializer[${Type.Name(className)}] {
           override def deserialize(p: com.fasterxml.jackson.core.JsonParser, ctxt: com.fasterxml.jackson.databind.DeserializationContext): ${Type.Name(
          className
        )} =
            ${Term.Name(className)}.from(p.${getter})
              .getOrElse({ throw new com.fasterxml.jackson.databind.JsonMappingException(p, s"Invalid value '$${p.${getter}}' for " + ${Lit
          .String(className)}) })
         }
       """
    )

  private def renderClass(className: String, tpe: scala.meta.Type, elems: RenderedEnum[ScalaLanguage]) = {
    val jacksSer =
      mod"@com.fasterxml.jackson.databind.annotation.JsonSerialize(using=classOf[${Type.Select(Term.Name(className), Type.Name(className + "Serializer"))}])"
    val jacksDeser =
      mod"@com.fasterxml.jackson.databind.annotation.JsonDeserialize(using=classOf[${Type.Select(Term.Name(className), Type.Name(className + "Deserializer"))}])"
    Target.pure(q"""
      ${jacksSer} ${jacksDeser} sealed abstract class ${Type.Name(className)}(val value: ${tpe}) extends _root_.scala.Product with _root_.scala.Serializable {
        override def toString: String = value.toString
      }
    """)
  }

  private def renderStaticDefns(
      className: String,
      tpe: scala.meta.Type,
      members: Option[scala.meta.Defn.Object],
      accessors: List[scala.meta.Term.Name],
      encoder: Option[scala.meta.Defn],
      decoder: Option[scala.meta.Defn]
  ): Target[StaticDefns[ScalaLanguage]] = {
    val longType = Type.Name(className)
    val terms: List[Defn.Val] = accessors.map { pascalValue =>
      q"val ${Pat.Var(pascalValue)}: ${longType} = members.${pascalValue}"
    }.toList
    val values: Defn.Val = q"val values = _root_.scala.Vector(..$accessors)"
    val implicits: List[Defn.Val] = List(
      q"implicit val ${Pat.Var(Term.Name(s"show${className}"))}: Show[${longType}] = Show[${tpe}].contramap[${longType}](_.value)"
    )
    Target.pure(
      StaticDefns[ScalaLanguage](
        className = className,
        extraImports = List.empty[Import],
        definitions = members.toList ++
          terms ++
          List(Some(values), encoder, decoder).flatten ++
          implicits ++
          List(
            q"def from(value: ${tpe}): _root_.scala.Option[${longType}] = values.find(_.value == value)",
            q"implicit val order: cats.Order[${longType}] = cats.Order.by[${longType}, Int](values.indexOf)"
          ) ++ List(
            q"implicit val ${Pat.Var(Term.Name(s"encode${className}"))}: GuardrailEncoder[$longType] = GuardrailEncoder.instance",
            q"implicit val ${Pat.Var(Term.Name(s"decode${className}"))}: GuardrailDecoder[$longType] = GuardrailDecoder.instance(new com.fasterxml.jackson.core.`type`.TypeReference[$longType] {})",
            q"implicit val ${Pat.Var(Term.Name(s"validate${className}"))}: GuardrailValidator[$longType] = GuardrailValidator.noop"
          ),
        statements = List.empty
      )
    )
  }

  override def buildAccessor(clsName: String, termName: String) =
    Target.pure(q"${Term.Name(clsName)}.${Term.Name(termName)}")

  private def extractProperties(spec: Tracker[Schema[_]]) =
    spec
      .refine[Target[List[(String, Tracker[Schema[_]])]]] { case ObjectExtractor(o) => o }(m =>
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
      meta: Either[core.LazyResolvedType[ScalaLanguage], core.Resolved[ScalaLanguage]],
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

        (tpe, classDep, rawType) <- meta match { // TODO: Target is not used
          case Right(core.Resolved(declType, classDep, _, rawType @ LiteralRawType(Some(rawTypeStr), rawFormat)))
              if isFile(rawTypeStr, rawFormat) && !isCustomType =>
            // assume that binary data are represented as a string. allow users to override.
            Target.pure((t"String", classDep, rawType))
          case Right(core.Resolved(declType, classDep, _, rawType)) =>
            Target.pure((declType, classDep, rawType))
          case Left(core.Deferred(tpeName)) =>
            val tpe = concreteTypes.find(_.clsName == tpeName).map(_.tpe).getOrElse {
              println(s"Unable to find definition for ${tpeName}, just inlining")
              Type.Name(tpeName)
            }
            Target.pure((tpe, Option.empty, fallbackRawType))
          case Left(core.DeferredArray(tpeName, containerTpe)) =>
            val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
            val innerType    = concreteType.getOrElse(Type.Name(tpeName))
            val tpe          = t"${containerTpe.getOrElse(t"_root_.scala.Vector")}[$innerType]"
            Target.pure((tpe, Option.empty, ReifiedRawType.ofVector(fallbackRawType)))
          case Left(core.DeferredMap(tpeName, customTpe)) =>
            val concreteType = lookupTypeName(tpeName, concreteTypes)(identity)
            val innerType    = concreteType.getOrElse(Type.Name(tpeName))
            val tpe          = t"${customTpe.getOrElse(t"_root_.scala.Predef.Map")}[_root_.scala.Predef.String, $innerType]"
            Target.pure((tpe, Option.empty, ReifiedRawType.ofMap(fallbackRawType)))
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
      className: String,
      supportPackage: List[String],
      selfParams: List[ProtocolParameter[ScalaLanguage]],
      parents: List[SuperClass[ScalaLanguage]] = Nil
  )(implicit Lt: LanguageTerms[ScalaLanguage, Target]) = {
    val discriminatorParams =
      parents.flatMap(parent => parent.discriminators.flatMap(discrim => parent.params.find(_.name.value == discrim.propertyName).map((discrim, _))))
    for {
      discriminators <- discriminatorParams.traverse { case (discriminator, param) =>
        for {
          discrimTpe <- Target.fromOption(param.term.decltpe, RuntimeFailure(s"Property ${param.name.value} has no type"))
          discrimValue <- JacksonHelpers
            .discriminatorExpression(
              param.name.value,
              discriminatorValue(discriminator, className),
              param.rawType
            )(
              v => Target.pure[Term](q"""BigInt(${Lit.String(v)})"""),
              v => Target.pure[Term](q"""BigDecimal(${Lit.String(v)})"""),
              v =>
                param.term.decltpe.fold(
                  Target.raiseUserError[Term](s"No declared type for property '${param.name.value}' on class $className")
                ) {
                  case tpe @ (_: Type.Name | _: Type.Select) =>
                    Lt.formatEnumName(v).map(ev => Term.Select(Term.Name(tpe.toString), Term.Name(ev)))
                  case tpe => Target.raiseError(RuntimeFailure(s"Assumed property ${param.name.value} was an enum, but can't handle $tpe"))
                }
            )
        } yield (param.name.value, param.term.name.value, param.term.decltpe, discrimValue)
      }
      presenceSerType        <- Lt.selectType(NonEmptyList.ofInitLast(supportPackage :+ "Presence", "PresenceSerializer"))
      presenceDeserType      <- Lt.selectType(NonEmptyList.ofInitLast(supportPackage :+ "Presence", "PresenceDeserializer"))
      optionNonNullDeserType <- Lt.selectType(NonEmptyList.ofInitLast(supportPackage :+ "Presence", "OptionNonNullDeserializer"))
      emptyIsNullDeserType   <- Lt.selectType(NonEmptyList.ofInitLast(supportPackage :+ "EmptyIsNullDeserializers", "EmptyIsNullDeserializer"))
      emptyIsNullOptionDeserType <- Lt.selectType(
        NonEmptyList.ofInitLast(supportPackage :+ "EmptyIsNullDeserializers", "EmptyIsNullOptionDeserializer")
      )
      optionNonMissingDeserType <- Lt.selectType(
        NonEmptyList.ofInitLast(supportPackage :+ "Presence", "OptionNonMissingDeserializer")
      )
      allTerms = selfParams ++ parents.flatMap(_.params)
      renderedClass = { // TODO: This logic should be reflowed. The scope and rebindings is due to a refactor where
        //       code from another dependent class was just copied in here wholesale.
        val discriminatorNames = parents.flatMap(_.discriminators).map(_.propertyName).toSet
        val params             = (parents.reverse.flatMap(_.params) ++ selfParams).filterNot(param => discriminatorNames.contains(param.term.name.value))
        val terms              = params.map(_.term)

        val toStringMethod = if (params.exists(_.dataRedaction != DataVisible)) {
          def mkToStringTerm(param: ProtocolParameter[ScalaLanguage]): Term = param match {
            case param if param.dataRedaction == DataVisible => q"${Term.Name(param.term.name.value)}.toString()"
            case _                                           => Lit.String("[redacted]")
          }

          val toStringTerms = params.map(p => List(mkToStringTerm(p))).intercalate(List(Lit.String(",")))

          List[Defn.Def](
            q"override def toString: String = ${toStringTerms.foldLeft[Term](Lit.String(s"${className}("))((accum, term) => q"$accum + $term")} + ${Lit.String(")")}"
          )
        } else {
          List.empty[Defn.Def]
        }

        val base = q"""$jsonIgnoreProperties case class ${Type.Name(className)}(..${terms.map(param =>
            allTerms
              .find(_.term.name.value == param.name.value)
              .fold(param) { term =>
                param.copy(
                  mods = paramAnnotations(
                    term,
                    presenceSerType,
                    presenceDeserType,
                    optionNonNullDeserType,
                    optionNonMissingDeserType,
                    emptyIsNullDeserType,
                    emptyIsNullOptionDeserType
                  ) ++ param.mods,
                  default = fixDefaultValue(term)
                )
              }
          )}) {
          ..${discriminators.map { case (propertyName, fieldName, tpe, value) =>
            q"""
                  @com.fasterxml.jackson.annotation.JsonProperty(${Lit.String(propertyName)})
                  val ${Pat.Var(Term.Name(fieldName))}: $tpe = $value
                """
          }};
          ..$toStringMethod
        }"""

        val parentOpt = if (parents.exists(s => s.discriminators.nonEmpty)) {
          parents.headOption
        } else {
          None
        }
        parentOpt
          .fold(base)(parent =>
            base.copy(
              templ = Template(
                Nil,
                (parent.clsName +: parent.interfaces).map(n => Init(Type.Name(n), Name(""), List.empty[Term.ArgClause])),
                Self(Name(""), None),
                base.templ.stats,
                Nil
              )
            )
          )
      }
    } yield renderedClass
  }

  private def encodeModel(
      clsName: String,
      dtoPackage: List[String],
      selfParams: List[ProtocolParameter[ScalaLanguage]],
      parents: List[SuperClass[ScalaLanguage]] = Nil
  ) = Target.pure(None)

  private def decodeModel(
      clsName: String,
      dtoPackage: List[String],
      supportPackage: List[String],
      selfParams: List[ProtocolParameter[ScalaLanguage]],
      parents: List[SuperClass[ScalaLanguage]] = Nil
  ): Target[Option[Defn.Val]] = Target.pure(None)

  private def renderDTOStaticDefns(
      className: String,
      deps: List[scala.meta.Term.Name],
      encoder: Option[scala.meta.Defn.Val],
      decoder: Option[scala.meta.Defn.Val],
      protocolParameters: List[ProtocolParameter[ScalaLanguage]]
  ) = {
    val extraImports: List[Import] = deps.map { term =>
      q"import ${term}._"
    }
    val classType       = Type.Name(className)
    val encoderInstance = q"implicit val ${Pat.Var(Term.Name(s"encode${className}"))}: GuardrailEncoder[$classType] = GuardrailEncoder.instance"
    val decoderInstance =
      q"implicit val ${Pat.Var(Term.Name(s"decode${className}"))}: GuardrailDecoder[$classType] = GuardrailDecoder.instance(new com.fasterxml.jackson.core.`type`.TypeReference[$classType] {})"
    val validatorInstance = q"implicit val ${Pat.Var(Term.Name(s"validate${className}"))}: GuardrailValidator[$classType] = GuardrailValidator.instance"
    Target.pure(
      StaticDefns[ScalaLanguage](
        className = className,
        extraImports = extraImports,
        definitions = (encoder ++ decoder ++ List(encoderInstance, decoderInstance, validatorInstance)).toList,
        statements = List.empty
      )
    )
  }

  private def extractArrayType(arr: Either[core.LazyResolvedType[ScalaLanguage], core.Resolved[ScalaLanguage]], concreteTypes: List[PropMeta[ScalaLanguage]]) =
    for {
      result <- arr match {
        case Right(core.Resolved(tpe, dep, default, _)) => Target.pure(tpe)
        case Left(core.Deferred(tpeName)) =>
          Target.fromOption(lookupTypeName(tpeName, concreteTypes)(identity), UserError(s"Unresolved reference ${tpeName}"))
        case Left(core.DeferredArray(tpeName, containerTpe)) =>
          Target.fromOption(
            lookupTypeName(tpeName, concreteTypes)(tpe => t"${containerTpe.getOrElse(t"_root_.scala.Vector")}[${tpe}]"),
            UserError(s"Unresolved reference ${tpeName}")
          )
        case Left(core.DeferredMap(tpeName, customTpe)) =>
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
        q"import cats.implicits._"
      )
    )

  override def staticProtocolImports(pkgName: List[String]): Target[List[Import]] = {
    val implicitsRef: Term.Ref = (pkgName.map(Term.Name.apply _) ++ List(q"Implicits")).foldLeft[Term.Ref](q"_root_")(Term.Select.apply _)
    Target.pure(
      List(
        q"import cats.implicits._",
        q"import cats.data.EitherT",
        q"import io.circe.refined._",
        q"import eu.timepit.refined.api.Refined",
        q"import eu.timepit.refined.auto._",
        q"import $implicitsRef._"
      )
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
                    import com.fasterxml.jackson.annotation.JsonInclude
                    import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, JsonToken}
                    import com.fasterxml.jackson.databind._
                    import com.fasterxml.jackson.databind.`type`.ReferenceType
                    import com.fasterxml.jackson.databind.deser.ContextualDeserializer
                    import com.fasterxml.jackson.databind.deser.std.StdDeserializer
                    import com.fasterxml.jackson.databind.jsontype.TypeSerializer
                    import com.fasterxml.jackson.databind.ser.ContextualSerializer
                    import com.fasterxml.jackson.databind.ser.std.ReferenceTypeSerializer
                    import com.fasterxml.jackson.databind.util.{AccessPattern, NameTransformer}

                    private class PresenceReferenceSerializer(
                        refType: ReferenceType,
                        ts: TypeSerializer,
                        ser: JsonSerializer[AnyRef]
                    ) extends ReferenceTypeSerializer[Presence[_]](refType, true, ts, ser) {
                      override def withResolved(prop: BeanProperty, vts: TypeSerializer, valueSer: JsonSerializer[_], unwrapper: NameTransformer): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, prop, vts, valueSer, unwrapper, _suppressableValue, _suppressNulls)
                      override def withContentInclusion(suppressableValue: AnyRef, suppressNulls: Boolean): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, _property, _valueTypeSerializer, _valueSerializer, _unwrapper, suppressableValue, suppressNulls)
                      override def _isValuePresent(value: Presence[_]): Boolean = value.toOption.isDefined
                      override def _getReferenced(value: Presence[_]): AnyRef = value.toOption.get.asInstanceOf[AnyRef]
                      override def _getReferencedIfPresent(value: Presence[_]): AnyRef = value.toOption.orNull[Any].asInstanceOf[AnyRef]
                    }

                    private class ResolvedPresenceSerializer(
                        base: ReferenceTypeSerializer[_],
                        property: BeanProperty,
                        vts: TypeSerializer,
                        valueSer: JsonSerializer[_],
                        unwrapper: NameTransformer,
                        suppressableValue: AnyRef,
                        suppressNulls: Boolean
                    ) extends ReferenceTypeSerializer[Presence[_]](base, property, vts, valueSer, unwrapper, suppressableValue, suppressNulls) {
                      override def withResolved(prop: BeanProperty, vts: TypeSerializer, valueSer: JsonSerializer[_], unwrapper: NameTransformer): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, prop, vts, valueSer, unwrapper, _suppressableValue, _suppressNulls)
                      override def withContentInclusion(suppressableValue: AnyRef, suppressNulls: Boolean): ReferenceTypeSerializer[Presence[_]] =
                        new ResolvedPresenceSerializer(this, _property, _valueTypeSerializer, _valueSerializer, _unwrapper, suppressableValue, suppressNulls)
                      override def _isValuePresent(value: Presence[_]): Boolean = value.toOption.isDefined
                      override def _getReferenced(value: Presence[_]): AnyRef = value.toOption.get.asInstanceOf[AnyRef]
                      override def _getReferencedIfPresent(value: Presence[_]): AnyRef = value.toOption.orNull[Any].asInstanceOf[AnyRef]
                    }

                    class PresenceSerializer extends JsonSerializer[Presence[_]] with ContextualSerializer {
                      override def serialize(value: Presence[_], gen: JsonGenerator, serializers: SerializerProvider): Unit =
                        throw new AssertionError("Contextual serializer must be used")

                      override def createContextual(prov: SerializerProvider, property: BeanProperty): JsonSerializer[_] = {
                        Option(property.getType.containedType(0)).fold({
                          throw new AssertionError("Presence type has no contained type")
                        })({
                          case tpe: ReferenceType => new PresenceReferenceSerializer(tpe, prov.findTypeSerializer(tpe), prov.findValueSerializer(tpe))
                          case tpe => new PresenceReferenceSerializer(ReferenceType.upgradeFrom(property.getType, tpe), prov.findTypeSerializer(tpe), prov.findValueSerializer(tpe))
                        })
                      }
                    }

                    private class PresenceOptionContextualDeserializer(innerType: JavaType) extends StdDeserializer[Presence[Option[Any]]](innerType) {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Presence[Option[Any]] =
                        Presence.present(Option(p.readValueAs(innerType.getRawClass)))

                      override def getNullValue(ctxt: DeserializationContext): Presence[Option[Any]] =
                        ctxt.getParser.getCurrentToken match {
                          case JsonToken.VALUE_NULL => Presence.present(None)
                          case _ => Presence.absent
                        }

                      override def getNullAccessPattern: AccessPattern = AccessPattern.DYNAMIC
                    }

                    private class PresenceContextualDeserializer(innerType: JavaType) extends StdDeserializer[Presence[_]](innerType) {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Presence[_] =
                        Presence.present(p.readValueAs(innerType.getRawClass))

                      override def getNullValue(ctxt: DeserializationContext): Presence[Option[_]] =
                        ctxt.getParser.getCurrentToken match {
                          case JsonToken.VALUE_NULL => throw new JsonMappingException(ctxt.getParser, s"null is not valid for '$${ctxt.getParser.getCurrentName}'")
                          case _ => Presence.absent
                        }

                      override def getNullAccessPattern: AccessPattern = AccessPattern.DYNAMIC
                    }

                    class PresenceDeserializer extends JsonDeserializer[Presence[_]] with ContextualDeserializer {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Presence[_] =
                        throw new AssertionError("Contextual deserializer must be used")

                      override def createContextual(ctxt: DeserializationContext, property: BeanProperty): JsonDeserializer[_] =
                        Option(ctxt.getContextualType.containedType(0)).flatMap({
                          case tpe if classOf[Option[_]].isAssignableFrom(tpe.getRawClass) =>
                            Option(tpe.containedType(0)).map(new PresenceOptionContextualDeserializer(_))
                          case tpe => Some(new PresenceContextualDeserializer(tpe))
                        }).getOrElse({
                          throw new JsonMappingException(ctxt.getParser, "Presence type has no contained type")
                        })
                    }

                    private class OptionContextualDeserializer(innerType: JavaType, nullValue: JsonToken => Option[_]) extends StdDeserializer[Option[_]](innerType) {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Option[_] =
                        Option(p.readValueAs(innerType.getRawClass))

                      override def getNullValue(ctxt: DeserializationContext): Option[_] = nullValue(ctxt.getParser.getCurrentToken)

                      override def getNullAccessPattern: AccessPattern = AccessPattern.DYNAMIC
                    }

                    class OptionNonNullDeserializer extends JsonDeserializer[Option[_]] with ContextualDeserializer {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Option[_] =
                        throw new AssertionError("Contextual serializer must be used")

                      override def createContextual(ctxt: DeserializationContext, property: BeanProperty): JsonDeserializer[_] =
                        Option(ctxt.getContextualType.containedType(0)).fold({
                          throw new JsonMappingException(ctxt.getParser, "Option type has no contained type")
                        })(
                          tpe => new OptionContextualDeserializer(tpe, {
                            case JsonToken.VALUE_NULL => throw new JsonMappingException(ctxt.getParser, s"null is not valid for '$${ctxt.getParser.getCurrentName}'")
                            case _ => None
                          })
                        )
                    }

                    class OptionNonMissingDeserializer extends JsonDeserializer[Option[_]] with ContextualDeserializer {
                      override def deserialize(p: JsonParser, ctxt: DeserializationContext): Option[_] =
                        throw new AssertionError("Contextual serializer must be used")

                      override def createContextual(ctxt: DeserializationContext, property: BeanProperty): JsonDeserializer[_] =
                        Option(ctxt.getContextualType.containedType(0)).fold({
                          throw new JsonMappingException(ctxt.getParser, "Option type has no contained type")
                        })(
                          tpe => new OptionContextualDeserializer(tpe, {
                            case JsonToken.VALUE_NULL => None
                            case _ => throw new JsonMappingException(ctxt.getParser, s"'$${ctxt.getParser.getCurrentName}' is missing")
                          })
                        )
                    }

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
    Target.pure(
      List(
        presenceDefinition,
        SupportDefinition[ScalaLanguage](
          q"EmptyIsNullDeserializers",
          List(
            q"import com.fasterxml.jackson.core.{JsonParser, JsonToken}",
            q"import com.fasterxml.jackson.databind.{DeserializationContext, JsonDeserializer, JsonMappingException}"
          ),
          List(
            q"""
                    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
                    object EmptyIsNullDeserializers {
                      class EmptyIsNullDeserializer extends JsonDeserializer[String] {
                        override def deserialize(p: JsonParser, ctxt: DeserializationContext): String =
                          p.currentToken() match {
                            case JsonToken.VALUE_STRING => p.getValueAsString match {
                              case "" => throw new JsonMappingException(p, "Value cannot be null")
                              case s => s
                            }
                            case _ => throw new JsonMappingException(p, s"Value is not a string")
                          }
                      }

                      class EmptyIsNullOptionDeserializer extends JsonDeserializer[Option[String]] {
                        override def deserialize(p: JsonParser, ctxt: DeserializationContext): Option[String] =
                          p.currentToken() match {
                            case JsonToken.VALUE_STRING => p.getValueAsString match {
                              case "" => None
                              case s => Option(s)
                            }
                            case _ => throw new JsonMappingException(p, s"Value is not a string")
                          }
                      }
                    }
                  """
          ),
          insideDefinitions = false
        )
      )
    )
  }

  private def packageObjectContents() =
    Target.pure(
      List.empty
    )

  private def implicitsObject() = Target.pure(
    Some(
      (
        q"JacksonImplicits",
        q"""
            object JacksonImplicits {
              object constraints {
                type NotNull = javax.validation.constraints.NotNull @scala.annotation.meta.field @scala.annotation.meta.param
              }

              trait GuardrailEncoder[A] {
                def encode(a: A)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper): com.fasterxml.jackson.databind.JsonNode =
                  mapper.valueToTree(a)
              }
              object GuardrailEncoder {
                def instance[A]: GuardrailEncoder[A] = new GuardrailEncoder[A] {}

                implicit def guardrailEncodeOption[B: GuardrailEncoder]: GuardrailEncoder[Option[B]] = instance
                implicit def guardrailEncodeVector[B: GuardrailEncoder]: GuardrailEncoder[Vector[B]] = instance
                implicit def guardrailEncodeMap[B: GuardrailEncoder]: GuardrailEncoder[Map[String, B]] = instance
                implicit val guardrailEncodeBoolean: GuardrailEncoder[Boolean] = instance
                implicit val guardrailEncodeInt: GuardrailEncoder[Int] = instance
                implicit val guardrailEncodeLong: GuardrailEncoder[Long] = instance
                implicit val guardrailEncodeBigInt: GuardrailEncoder[BigInt] = instance
                implicit val guardrailEncodeFloat: GuardrailEncoder[Float] = instance
                implicit val guardrailEncodeDouble: GuardrailEncoder[Double] = instance
                implicit val guardrailEncodeBigDecimal: GuardrailEncoder[BigDecimal] = instance
                implicit val guardrailEncodeString: GuardrailEncoder[String] = instance
                implicit val guardrailEncodeBase64String: GuardrailEncoder[Base64String] = instance
                implicit val guardrailEncodeInstant: GuardrailEncoder[java.time.Instant] = instance
                implicit val guardrailEncodeLocalDate: GuardrailEncoder[java.time.LocalDate] = instance
                implicit val guardrailEncodeLocalDateTime: GuardrailEncoder[java.time.LocalDateTime] = instance
                implicit val guardrailEncodeLocalTime: GuardrailEncoder[java.time.LocalTime] = instance
                implicit val guardrailEncodeOffsetDateTime: GuardrailEncoder[java.time.OffsetDateTime] = instance
                implicit val guardrailEncodeZonedDateTime: GuardrailEncoder[java.time.ZonedDateTime] = instance
              }

              trait GuardrailDecoder[A] {
                def tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[A], Class[A]]
                def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[A]): scala.util.Try[A] =
                  scala.util.Try(this.tpe.fold(mapper.convertValue(jsonNode, _), mapper.convertValue(jsonNode, _))).flatMap(guardrailValidator.validate)
              }
              object GuardrailDecoder {
                def instance[A](typeRef: com.fasterxml.jackson.core.`type`.TypeReference[A]): GuardrailDecoder[A] = new GuardrailDecoder[A] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[A], Class[A]] = Left(typeRef)
                }
                def instance[A](cls: Class[A]): GuardrailDecoder[A] = new GuardrailDecoder[A] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[A], Class[A]] = Right(cls)
                }

                implicit def guardrailDecodeOption[B: GuardrailDecoder: GuardrailValidator]: GuardrailDecoder[Option[B]] = new GuardrailDecoder[Option[B]] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[Option[B]], Class[Option[B]]] = Left(new com.fasterxml.jackson.core.`type`.TypeReference[Option[B]] {})
                  override def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[Option[B]]): scala.util.Try[Option[B]] = {
                    if (jsonNode.isNull) {
                      scala.util.Success(Option.empty[B])
                    } else {
                      implicitly[GuardrailDecoder[B]].decode(jsonNode).map(Option.apply)
                    }
                  }
                }
                implicit def guardrailDecodeVector[B: GuardrailDecoder: GuardrailValidator]: GuardrailDecoder[Vector[B]] = new GuardrailDecoder[Vector[B]] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[Vector[B]], Class[Vector[B]]] = Left(new com.fasterxml.jackson.core.`type`.TypeReference[Vector[B]] {})
                  override def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[Vector[B]]): scala.util.Try[Vector[B]] = {
                    jsonNode match {
                      case arr: com.fasterxml.jackson.databind.node.ArrayNode =>
                        import cats.implicits._
                        import _root_.scala.jdk.CollectionConverters._
                        arr.iterator().asScala.toVector.traverse(implicitly[GuardrailDecoder[B]].decode)
                      case _ =>
                        scala.util.Failure(new com.fasterxml.jackson.databind.JsonMappingException(null, s"Can't decode to vector; node of type $${jsonNode.getClass.getSimpleName} is not an array"))
                    }
                  }
                }
                implicit def guardrailDecodeMap[B: GuardrailDecoder: GuardrailValidator]: GuardrailDecoder[Map[String, B]] = new GuardrailDecoder[Map[String, B]] {
                  override val tpe: Either[com.fasterxml.jackson.core.`type`.TypeReference[Map[String, B]], Class[Map[String, B]]] = Left(new com.fasterxml.jackson.core.`type`.TypeReference[Map[String, B]] {})
                  override def decode(jsonNode: com.fasterxml.jackson.databind.JsonNode)(implicit mapper: com.fasterxml.jackson.databind.ObjectMapper, validator: javax.validation.Validator, guardrailValidator: GuardrailValidator[Map[String, B]]): scala.util.Try[Map[String, B]] = {
                    jsonNode match {
                      case obj: com.fasterxml.jackson.databind.node.ObjectNode =>
                        import cats.implicits._
                        import _root_.scala.jdk.CollectionConverters._
                        obj.fields().asScala.toVector.traverse(entry => implicitly[GuardrailDecoder[B]].decode(entry.getValue).map((entry.getKey, _))).map(_.toMap)
                      case _ =>
                        scala.util.Failure(new com.fasterxml.jackson.databind.JsonMappingException(null, s"Can't decode to map; node of type $${jsonNode.getClass.getSimpleName} is not an object"))
                    }
                  }
                }
                implicit val guardrailDecodeBoolean: GuardrailDecoder[Boolean] = instance(classOf[Boolean])
                implicit val guardrailDecodeInt: GuardrailDecoder[Int] = instance(classOf[Int])
                implicit val guardrailDecodeLong: GuardrailDecoder[Long] = instance(classOf[Long])
                implicit val guardrailDecodeBigInt: GuardrailDecoder[BigInt] = instance(classOf[BigInt])
                implicit val guardrailDecodeFloat: GuardrailDecoder[Float] = instance(classOf[Float])
                implicit val guardrailDecodeDouble: GuardrailDecoder[Double] = instance(classOf[Double])
                implicit val guardrailDecodeBigDecimal: GuardrailDecoder[BigDecimal] = instance(classOf[BigDecimal])
                implicit val guardrailDecodeString: GuardrailDecoder[String] = instance(classOf[String])
                implicit val guardrailDecodeBase64String: GuardrailDecoder[Base64String] = instance(classOf[Base64String])
                implicit val guardrailDecodeInstant: GuardrailDecoder[java.time.Instant] = instance(classOf[java.time.Instant])
                implicit val guardrailDecodeLocalDate: GuardrailDecoder[java.time.LocalDate] = instance(classOf[java.time.LocalDate])
                implicit val guardrailDecodeLocalDateTime: GuardrailDecoder[java.time.LocalDateTime] = instance(classOf[java.time.LocalDateTime])
                implicit val guardrailDecodeLocalTime: GuardrailDecoder[java.time.LocalTime] = instance(classOf[java.time.LocalTime])
                implicit val guardrailDecodeOffsetDateTime: GuardrailDecoder[java.time.OffsetDateTime] = instance(classOf[java.time.OffsetDateTime])
                implicit val guardrailDecodeZonedDateTime: GuardrailDecoder[java.time.ZonedDateTime] = instance(classOf[java.time.ZonedDateTime])
              }

              trait GuardrailValidator[A] {
                def validate(a: A)(implicit validator: javax.validation.Validator): scala.util.Try[A]
              }
              object GuardrailValidator {
                def instance[A]: GuardrailValidator[A] = new GuardrailValidator[A] {
                  override def validate(a: A)(implicit validator: javax.validation.Validator): scala.util.Try[A] = {
                    import _root_.scala.jdk.CollectionConverters._
                    scala.util.Try(validator.validate(a)).flatMap({
                      case violations if violations.isEmpty =>
                        scala.util.Success(a)
                      case violations =>
                        scala.util.Failure(new javax.validation.ValidationException(s"Validation of $${a.getClass.getSimpleName} failed: $${violations.asScala.map(viol => s"$${viol.getPropertyPath}: $${viol.getMessage}").mkString("; ")}"))
                    })
                  }
                }
                def noop[A]: GuardrailValidator[A] = new GuardrailValidator[A] {
                  override def validate(a: A)(implicit validator: javax.validation.Validator): scala.util.Try[A] = scala.util.Success(a)
                }

                implicit def guardrailValidateOption[A: GuardrailValidator]: GuardrailValidator[Option[A]] = new GuardrailValidator[Option[A]] {
                  override def validate(a: Option[A])(implicit validator: javax.validation.Validator): scala.util.Try[Option[A]] =
                    a.traverse(implicitly[GuardrailValidator[A]].validate)
                }
                implicit def guardrailValidateVector[A: GuardrailValidator]: GuardrailValidator[Vector[A]] = new GuardrailValidator[Vector[A]] {
                  override def validate(a: Vector[A])(implicit validator: javax.validation.Validator): scala.util.Try[Vector[A]] =
                    a.traverse(implicitly[GuardrailValidator[A]].validate)
                }
                implicit def guardrailValidateMap[A: GuardrailValidator]: GuardrailValidator[Map[String, A]] = new GuardrailValidator[Map[String, A]] {
                  override def validate(a: Map[String, A])(implicit validator: javax.validation.Validator): scala.util.Try[Map[String, A]] =
                    a.toVector.traverse({ case (k, v) => implicitly[GuardrailValidator[A]].validate(v).map((k, _)) }).map(_.toMap)
                }
                implicit val guardrailValidateBoolean: GuardrailValidator[Boolean] = noop
                implicit val guardrailValidateInt: GuardrailValidator[Int] = noop
                implicit val guardrailValidateLong: GuardrailValidator[Long] = noop
                implicit val guardrailValidateBigInt: GuardrailValidator[BigInt] = noop
                implicit val guardrailValidateFloat: GuardrailValidator[Float] = noop
                implicit val guardrailValidateDouble: GuardrailValidator[Double] = noop
                implicit val guardrailValidateBigDecimal: GuardrailValidator[BigDecimal] = noop
                implicit val guardrailValidateString: GuardrailValidator[String] = instance
                implicit val guardrailValidateBase64String: GuardrailValidator[Base64String] = instance
                implicit val guardrailValidateInstant: GuardrailValidator[java.time.Instant] = noop
                implicit val guardrailValidateLocalDate: GuardrailValidator[java.time.LocalDate] = noop
                implicit val guardrailValidateLocalDateTime: GuardrailValidator[java.time.LocalDateTime] = noop
                implicit val guardrailValidateLocalTime: GuardrailValidator[java.time.LocalTime] = noop
                implicit val guardrailValidateOffsetDateTime: GuardrailValidator[java.time.OffsetDateTime] = noop
                implicit val guardrailValidateZonedDateTime: GuardrailValidator[java.time.ZonedDateTime] = noop
              }
            }
         """
      )
    )
  )

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
      className: String,
      discriminator: Discriminator[ScalaLanguage],
      encoder: Option[scala.meta.Defn.Val],
      decoder: Option[scala.meta.Defn.Val]
  ) = {
    val classType = Type.Name(className)
    Target.pure(
      StaticDefns[ScalaLanguage](
        className = className,
        extraImports = List.empty[Import],
        definitions = List[Option[Defn]](
          Some(q"val discriminator: String = ${Lit.String(discriminator.propertyName)}"),
          encoder, // TODO: encoder/decoder and the following three defns look _very_ suspicious. Evaluate whether they need to move into the emitter methods.
          decoder,
          Some(q"implicit val ${Pat.Var(Term.Name(s"encode${className}"))}: GuardrailEncoder[$classType] = GuardrailEncoder.instance"),
          Some(
            q"implicit val ${Pat.Var(Term.Name(s"decode${className}"))}: GuardrailDecoder[$classType] = GuardrailDecoder.instance(new com.fasterxml.jackson.core.`type`.TypeReference[$classType] {})"
          ),
          Some(q"implicit val ${Pat.Var(Term.Name(s"validate${className}"))}: GuardrailValidator[$classType] = GuardrailValidator.instance")
        ).flatten,
        statements = List.empty
      )
    )
  }

  private def decodeADT(clsName: String, discriminator: Discriminator[ScalaLanguage], children: List[String] = Nil) =
    Target.pure(None)

  private def encodeADT(clsName: String, discriminator: Discriminator[ScalaLanguage], children: List[String] = Nil) =
    Target.pure(None)

  private def renderSealedTrait(
      className: String,
      params: List[ProtocolParameter[ScalaLanguage]],
      discriminator: Discriminator[ScalaLanguage],
      parents: List[SuperClass[ScalaLanguage]] = Nil,
      children: List[String] = Nil
  ) = {
    val parent = for {
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
    for {
      renderedTrait      <- parent
      discriminatorParam <- Target.pure(params.find(_.name.value == discriminator.propertyName))
    } yield {
      val subTypes = children.map(child =>
        q"new com.fasterxml.jackson.annotation.JsonSubTypes.Type(name = ${Lit.String(discriminatorValue(discriminator, child))}, value = classOf[${Type.Name(child)}])"
      )

      renderedTrait.copy(
        mods = List(
          mod"""
        @com.fasterxml.jackson.annotation.JsonTypeInfo(
          use = com.fasterxml.jackson.annotation.JsonTypeInfo.Id.NAME,
          include = com.fasterxml.jackson.annotation.JsonTypeInfo.As.PROPERTY,
          property = ${Lit.String(discriminator.propertyName)}
        )
        """,
          mod"""
        @com.fasterxml.jackson.annotation.JsonSubTypes(Array(
          ..$subTypes
        ))
         """
        ) ++ renderedTrait.mods,
        templ = renderedTrait.templ.copy(
          stats =
            renderedTrait.templ.stats ++ discriminatorParam.map(param => q"def ${Term.Name(param.term.name.value)}: ${param.term.decltpe.getOrElse(t"Any")}")
        )
      )
    }
  }
}
