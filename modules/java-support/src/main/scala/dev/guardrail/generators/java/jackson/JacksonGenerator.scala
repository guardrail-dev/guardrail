package dev.guardrail.generators.java.jackson

import _root_.io.swagger.v3.oas.models.{ Components, OpenAPI }
import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, _ }

import cats.Foldable
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.{ FlatMap, Monad }

import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.Modifier.Keyword.{ FINAL, PRIVATE, PROTECTED, PUBLIC }
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, UnknownType }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr.{ MethodCallExpr, _ }
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.{ Node, NodeList }
import scala.jdk.CollectionConverters._
import scala.reflect.runtime.universe.typeTag

import dev.guardrail.core
import dev.guardrail.core.resolvers.ModelResolver
import dev.guardrail.core.extract.{ CustomArrayTypeName, CustomMapTypeName, CustomTypeName, DataRedaction, Default, EmptyValueIsNull }
import dev.guardrail.core.implicits._
import dev.guardrail.core.{
  DataRedacted,
  DataVisible,
  EmptyIsEmpty,
  EmptyIsNull,
  EmptyToNullBehaviour,
  LiteralRawType,
  Mappish,
  RedactionBehaviour,
  ReifiedRawType,
  Tracker
}
import dev.guardrail.generators.ProtocolDefinitions
import dev.guardrail.generators.ProtocolGenerator.{ WrapEnumSchema, wrapNumberEnumSchema, wrapObjectEnumSchema, wrapStringEnumSchema }
import dev.guardrail.generators.RawParameterName
import dev.guardrail.generators.java.JavaCollectionsGenerator
import dev.guardrail.generators.java.JavaGenerator
import dev.guardrail.generators.java.JavaLanguage
import dev.guardrail.generators.java.JavaVavrCollectionsGenerator
import dev.guardrail.generators.java.syntax._
import dev.guardrail.generators.protocol.{ ClassChild, ClassHierarchy, ClassParent }
import dev.guardrail.generators.spi.{ CollectionsGeneratorLoader, ModuleLoadResult, ProtocolGeneratorLoader }
import dev.guardrail.terms.collections.{ CollectionsAbstraction, JavaStdLibCollections, JavaVavrCollections }
import dev.guardrail.terms.framework.FrameworkTerms
import dev.guardrail.terms.protocol.{ Discriminator, EnumDefinition, PropertyRequirement }
import dev.guardrail.terms.protocol._
import dev.guardrail.terms.{
  CollectionsLibTerms,
  HeldEnum,
  IntHeldEnum,
  LanguageTerms,
  LongHeldEnum,
  ProtocolTerms,
  RenderedEnum,
  RenderedIntEnum,
  RenderedLongEnum,
  RenderedStringEnum,
  StringHeldEnum,
  SwaggerTerms
}
import dev.guardrail.{ RuntimeFailure, Target, UserError }

class JacksonProtocolGeneratorLoader extends ProtocolGeneratorLoader {
  type L = JavaLanguage
  def reified = typeTag[Target[JavaLanguage]]

  def apply =
    ModuleLoadResult.forProduct3(
      ProtocolGeneratorLoader.label    -> Seq(JacksonVersion.mapping),
      CollectionsGeneratorLoader.label -> Seq(JavaStdLibCollections.mapping, JavaVavrCollections.mapping),
      CollectionsGeneratorLoader.label -> Seq(JavaCollectionsGenerator.mapping, JavaVavrCollectionsGenerator.mapping)
    )((_, ca, cl) => JacksonGenerator()(cl, ca))
}

object JacksonGenerator {
  def apply()(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage]): ProtocolTerms[JavaLanguage, Target] =
    new JacksonGenerator
}

@SuppressWarnings(Array("org.wartremover.warts.Null"))
class JacksonGenerator private (implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage])
    extends ProtocolTerms[JavaLanguage, Target] {
  import Ca._

  override implicit def MonadF: Monad[Target] = Target.targetInstances

  import Target.targetInstances // TODO: Remove me. This resolves implicit ambiguity from MonadChain

  private val BUILDER_TYPE        = StaticJavaParser.parseClassOrInterfaceType("Builder")
  private val BIG_INTEGER_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigInteger")
  private val BIG_DECIMAL_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigDecimal")

  private case class ParameterTerm(
      propertyName: String,
      parameterName: String,
      fieldType: Type,
      parameterType: Type,
      rawType: ReifiedRawType,
      defaultValue: Option[Expression],
      dataRedacted: RedactionBehaviour,
      emptyToNull: EmptyToNullBehaviour
  )

  private def groupHierarchies(
      definitions: Mappish[List, String, Tracker[Schema[_]]]
  )(implicit
      Sc: LanguageTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[(List[ClassParent[JavaLanguage]], List[(String, Tracker[Schema[_]])])] = {

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

    def children(cls: String): List[ClassChild[JavaLanguage]] = definitions.value.flatMap { case (clsName, comp) =>
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

    def classHierarchy(cls: String, model: Tracker[Schema[_]]): Target[Option[ClassParent[JavaLanguage]]] =
      model
        .refine { case c: ComposedSchema => c }(c =>
          firstInHierarchy(c)
            .fold(Option.empty[Discriminator[JavaLanguage]].pure[Target])(Discriminator.fromSchema[JavaLanguage, Target])
            .map(_.map((_, getRequiredFieldsRec(c))))
        )
        .orRefine { case x: Schema[_] => x }(m => Discriminator.fromSchema(m).map(_.map((_, getRequiredFieldsRec(m)))))
        .getOrElse(Option.empty[(Discriminator[JavaLanguage], List[String])].pure[Target])
        .map(_.map { case (discriminator, reqFields) => ClassParent(cls, model, children(cls), discriminator, reqFields) })

    Sw.log.function("groupHierarchies")(
      definitions.value
        .traverse { case (cls, model) =>
          for {
            hierarchy <- classHierarchy(cls, model)
          } yield hierarchy.filterNot(_.children.isEmpty).toLeft((cls, model))
        }
        .map(_.partitionEither[ClassParent[JavaLanguage], (String, Tracker[Schema[_]])](identity))
    )
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
      P: ProtocolTerms[JavaLanguage, Target],
      F: FrameworkTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target],
      wrapEnumSchema: WrapEnumSchema[A]
  ): Target[Either[String, EnumDefinition[JavaLanguage]]] = {
    import Cl._
    import Sc._
    import Sw._

    def validProg(held: HeldEnum, tpe: Type, fullType: Type): Target[EnumDefinition[JavaLanguage]] =
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
              wrappedValues = RenderedStringEnum[JavaLanguage](elems)
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
              wrappedValues = RenderedIntEnum[JavaLanguage](elems)
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
              wrappedValues = RenderedLongEnum[JavaLanguage](elems)
            } yield (pascalValues, wrappedValues)
        }
        members <- renderMembers(clsName, wrappedValues)
        encoder <- encodeEnum(clsName, tpe)
        decoder <- decodeEnum(clsName, tpe)

        defn        <- renderClass(clsName, tpe, wrappedValues)
        staticDefns <- renderStaticDefns(clsName, tpe, members, pascalValues, encoder, decoder)
        classType   <- pureTypeName(clsName)
      } yield EnumDefinition[JavaLanguage](clsName, classType, fullType, wrappedValues, defn, staticDefns)

    for {
      enum     <- extractEnum(schema.map(wrapEnumSchema))
      prefixes <- vendorPrefixes()
      (tpe, _) <- ModelResolver.determineTypeName(schema, Tracker.cloneHistory(schema, CustomTypeName(schema, prefixes)), components)
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

  private def defaultValue(
      name: NonEmptyList[String],
      schema: Tracker[Schema[_]],
      requirement: PropertyRequirement,
      definitions: List[(String, Tracker[Schema[_]])]
  )(implicit
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target]
  ): Target[Option[Node]] = {
    import Sc._
    import Cl._
    val empty = Option.empty[Node].pure[Target]
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

  /** Handle polymorphic model
    */
  private[this] def fromPoly(
      hierarchy: ClassParent[JavaLanguage],
      concreteTypes: List[PropMeta[JavaLanguage]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[JavaLanguage, Target],
      P: ProtocolTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[ProtocolElems[JavaLanguage]] = {
    import Cl._
    import Sc._

    def child(hierarchy: ClassHierarchy[JavaLanguage]): List[String] =
      hierarchy.children.map(_.name) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy[JavaLanguage]): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.name :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator

    for {
      parents <- hierarchy.model
        .refine[Target[List[SuperClass[JavaLanguage]]]] { case c: ComposedSchema => c }(
          extractParents(_, definitions, concreteTypes, dtoPackage, supportPackage, defaultPropertyRequirement, components)
        )
        .getOrElse(List.empty[SuperClass[JavaLanguage]].pure[Target])
      props <- extractProperties(hierarchy.model)
      requiredFields = hierarchy.required ::: hierarchy.children.flatMap(_.required)
      params <- props.traverse { case (name, prop) =>
        for {
          typeName <- formatTypeName(name).map(formattedName => NonEmptyList.of(hierarchy.name, formattedName))
          propertyRequirement = getPropertyRequirement(prop, requiredFields.contains(name), defaultPropertyRequirement)
          prefixes <- vendorPrefixes()
          resolvedType <- ModelResolver
            .propMeta[JavaLanguage, Target](
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
    } yield ADT[JavaLanguage](
      name = hierarchy.name,
      tpe = tpe,
      fullType = fullType,
      trt = definition,
      staticDefns = staticDefns
    )
  }

  private def prepareProperties(
      clsName: NonEmptyList[String],
      propertyToTypeLookup: Map[String, String],
      props: List[(String, Tracker[Schema[_]])],
      requiredFields: List[String],
      concreteTypes: List[PropMeta[JavaLanguage]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[JavaLanguage, Target],
      P: ProtocolTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[(List[ProtocolParameter[JavaLanguage]], List[NestedProtocolElems[JavaLanguage]])] = {
    import Cl._
    import Sc._
    def getClsName(name: String): NonEmptyList[String] = propertyToTypeLookup.get(name).map(NonEmptyList.of(_)).getOrElse(clsName)

    def processProperty(name: String, schema: Tracker[Schema[_]]): Target[Option[Either[String, NestedProtocolElems[JavaLanguage]]]] =
      for {
        nestedClassName <- formatTypeName(name).map(formattedName => getClsName(name).append(formattedName))
        defn <- schema
          .refine[Target[Option[Either[String, NestedProtocolElems[JavaLanguage]]]]] { case x: ObjectSchema => x }(o =>
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
            fromEnum[String](nestedClassName.last, s, dtoPackage, components).map(Option(_))
          )
          .getOrElse(Option.empty[Either[String, NestedProtocolElems[JavaLanguage]]].pure[Target])
      } yield defn

    for {
      paramsAndNestedDefinitions <- props.traverse[Target, (Tracker[ProtocolParameter[JavaLanguage]], Option[NestedProtocolElems[JavaLanguage]])] {
        case (name, schema) =>
          for {
            typeName              <- formatTypeName(name).map(formattedName => getClsName(name).append(formattedName))
            tpe                   <- selectType(typeName)
            maybeNestedDefinition <- processProperty(name, schema)
            resolvedType          <- ModelResolver.propMetaWithName(tpe, schema, components)
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
      params: List[Tracker[ProtocolParameter[JavaLanguage]]]
  )(implicit Sw: SwaggerTerms[JavaLanguage, Target], Sc: LanguageTerms[JavaLanguage, Target]): Target[List[ProtocolParameter[JavaLanguage]]] = {
    import Sc._
    Foldable[List]
      .foldLeftM[Target, Tracker[ProtocolParameter[JavaLanguage]], List[ProtocolParameter[JavaLanguage]]](params, List.empty[ProtocolParameter[JavaLanguage]]) {
        (s, ta) =>
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
                val mergedParameter = ProtocolParameter[JavaLanguage](
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
      params: List[ProtocolParameter[JavaLanguage]]
  )(implicit Lt: LanguageTerms[JavaLanguage, Target]): Target[List[ProtocolParameter[JavaLanguage]]] = {
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
      Fw: FrameworkTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[ProtocolElems[JavaLanguage]] = {
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
      prefixes <- vendorPrefixes()
      tpe <- model.fold[Target[Type]](objectType(None)) { m =>
        for {
          (declType, _) <- ModelResolver.determineTypeName[JavaLanguage, Target](m, Tracker.cloneHistory(m, CustomTypeName(m, prefixes)), components)
        } yield declType
      }
      res <- typeAlias(clsName, tpe)
    } yield res
  }

  private def plainTypeAlias(
      clsName: String
  )(implicit Fw: FrameworkTerms[JavaLanguage, Target], Sc: LanguageTerms[JavaLanguage, Target]): Target[ProtocolElems[JavaLanguage]] = {
    import Fw._
    for {
      tpe <- objectType(None)
      res <- typeAlias(clsName, tpe)
    } yield res
  }

  private def typeAlias(clsName: String, tpe: Type): Target[ProtocolElems[JavaLanguage]] =
    (RandomType[JavaLanguage](clsName, tpe): ProtocolElems[JavaLanguage]).pure[Target]

  private def fromArray(clsName: String, arr: Tracker[ArraySchema], concreteTypes: List[PropMeta[JavaLanguage]], components: Tracker[Option[Components]])(
      implicit
      F: FrameworkTerms[JavaLanguage, Target],
      P: ProtocolTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[ProtocolElems[JavaLanguage]] =
    for {
      deferredTpe <- ModelResolver.modelMetaType(arr, components)
      tpe         <- extractArrayType(deferredTpe, concreteTypes)
      ret         <- typeAlias(clsName, tpe)
    } yield ret

  private def extractParents(
      elem: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])],
      concreteTypes: List[PropMeta[JavaLanguage]],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[JavaLanguage, Target],
      P: ProtocolTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[List[SuperClass[JavaLanguage]]] = {
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
            _.refine[Target[List[Discriminator[JavaLanguage]]]] { case m: ObjectSchema => m }(m => Discriminator.fromSchema(m).map(_.toList))
              .getOrElse(List.empty[Discriminator[JavaLanguage]].pure[Target])
          )
        } yield tpe
          .map(
            SuperClass[JavaLanguage](
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
      parents: List[SuperClass[JavaLanguage]],
      concreteTypes: List[PropMeta[JavaLanguage]],
      definitions: List[(String, Tracker[Schema[_]])],
      dtoPackage: List[String],
      supportPackage: List[String],
      defaultPropertyRequirement: PropertyRequirement,
      components: Tracker[Option[Components]]
  )(implicit
      F: FrameworkTerms[JavaLanguage, Target],
      P: ProtocolTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[Either[String, ClassDefinition[JavaLanguage]]] = {
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
        case classDefinition: ClassDefinition[JavaLanguage] =>
          for {
            widenClass          <- widenClassDefinition(classDefinition.cls)
            companionTerm       <- pureTermName(classDefinition.name)
            companionDefinition <- wrapToObject(companionTerm, classDefinition.staticDefns.extraImports, classDefinition.staticDefns.definitions)
            widenCompanion      <- companionDefinition.traverse(widenObjectDefinition)
          } yield List(widenClass) ++ widenCompanion.fold(classDefinition.staticDefns.definitions)(List(_))
        case enumDefinition: EnumDefinition[JavaLanguage] =>
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
      if (parents.isEmpty && props.isEmpty) Left("Entity isn't model"): Either[String, ClassDefinition[JavaLanguage]]
      else tpe.toRight("Empty entity name").map(ClassDefinition[JavaLanguage](clsName.last, _, fullType, defn, finalStaticDefns, parents))
    }
  }

  override def fromSpec(
      spec: Tracker[OpenAPI],
      dtoPackage: List[String],
      supportPackage: NonEmptyList[String],
      defaultPropertyRequirement: PropertyRequirement
  )(implicit
      F: FrameworkTerms[JavaLanguage, Target],
      P: ProtocolTerms[JavaLanguage, Target],
      Sc: LanguageTerms[JavaLanguage, Target],
      Cl: CollectionsLibTerms[JavaLanguage, Target],
      Sw: SwaggerTerms[JavaLanguage, Target]
  ): Target[ProtocolDefinitions[JavaLanguage]] = {
    import Cl._
    import Sc._

    val components  = spec.downField("components", _.getComponents())
    val definitions = components.flatDownField("schemas", _.getSchemas()).indexedCosequence
    Sw.log.function("ProtocolGenerator.fromSpec")(for {
      (hierarchies, definitionsWithoutPoly) <- groupHierarchies(definitions)

      concreteTypes <- PropMeta.extractConcreteTypes[JavaLanguage, Target](definitions.value, components)
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
              (declType, _) <- ModelResolver.determineTypeName[JavaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
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
              (declType, _) <- ModelResolver.determineTypeName[JavaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
              alias         <- typeAlias(formattedClsName, declType)
            } yield enum.orElse(model).getOrElse(alias)
          )
          .valueOr(x =>
            for {
              formattedClsName <- formatTypeName(clsName)
              (declType, _)    <- ModelResolver.determineTypeName[JavaLanguage, Target](x, Tracker.cloneHistory(x, CustomTypeName(x, prefixes)), components)
              res              <- typeAlias(formattedClsName, declType)
            } yield res
          )
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()
      implicitsObject   <- implicitsObject()

      polyADTElems <- ProtocolElems.resolve[JavaLanguage, Target](polyADTs)
      strictElems  <- ProtocolElems.resolve[JavaLanguage, Target](elems)
    } yield ProtocolDefinitions[JavaLanguage](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents, implicitsObject))
  }

  // returns a tuple of (requiredTerms, optionalTerms)
  // note that required terms _that have a default value_ are conceptually optional.
  private def sortParams(
      params: List[ProtocolParameter[JavaLanguage]]
  )(implicit Ca: CollectionsAbstraction[JavaLanguage]): (List[ParameterTerm], List[ParameterTerm]) = {
    import Ca._

    def defaultValueToExpression(defaultValue: Option[Node]): Option[Expression] = defaultValue match {
      case Some(expr: Expression) => Some(expr)
      case _                      => None
    }

    params
      .map { param =>
        val parameterType = if (param.term.getType.isOptionalType) {
          param.term.getType.containedType.unbox
        } else {
          param.term.getType.unbox
        }
        val defaultValue = defaultValueToExpression(param.defaultValue)

        ParameterTerm(
          param.name.value,
          param.term.getNameAsString,
          param.term.getType.unbox,
          parameterType,
          param.rawType,
          defaultValue,
          param.dataRedaction,
          param.emptyToNull
        )
      }
      .partition(pt => !pt.fieldType.isOptionalType && pt.defaultValue.isEmpty)
  }

  private def addParents(cls: ClassOrInterfaceDeclaration, parentOpt: Option[SuperClass[JavaLanguage]]): Unit =
    parentOpt.foreach { parent =>
      val directParent = StaticJavaParser.parseClassOrInterfaceType(parent.clsName)
      val otherParents = parent.interfaces.map(StaticJavaParser.parseClassOrInterfaceType)
      val _ = cls
        .setExtendedTypes(new NodeList(directParent))
        .setImplementedTypes(otherParents.toNodeList)
    }

  private def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[JavaLanguage]]): Option[Type] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)

  private def addParameterGetter(cls: ClassOrInterfaceDeclaration, param: ParameterTerm): Unit = {
    val _ = cls
      .addMethod(getterMethodNameForParameter(param.parameterName), PUBLIC)
      .setType(param.fieldType)
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new FieldAccessExpr(new ThisExpr, param.parameterName))
          )
        )
      )
  }

  private def dtoConstructorBody(
      superCall: Expression,
      terms: List[ParameterTerm]
  )(implicit Cl: CollectionsLibTerms[JavaLanguage, Target], Ca: CollectionsAbstraction[JavaLanguage], fm: FlatMap[Target]): Target[BlockStmt] = {
    import Ca._
    for {
      emptyOptional <- Cl.emptyOptionalTerm().flatMap(_.toExpression)
    } yield new BlockStmt(
      (
        List[Statement](new ExpressionStmt(superCall)) ++
          terms
            .map(term =>
              new ExpressionStmt(
                new AssignExpr(
                  new FieldAccessExpr(new ThisExpr, term.parameterName),
                  if (term.fieldType.isOptionalType) {
                    val parameterValueExpr = term.emptyToNull match {
                      case EmptyIsEmpty => new NameExpr(term.parameterName)
                      case EmptyIsNull =>
                        new MethodCallExpr(
                          new NameExpr(term.parameterName),
                          "filter",
                          new NodeList[Expression](
                            new LambdaExpr(
                              new Parameter(new UnknownType, term.parameterName + "_"),
                              new UnaryExpr(
                                new MethodCallExpr(
                                  new StringLiteralExpr(""),
                                  "equals",
                                  new NodeList[Expression](new NameExpr(term.parameterName + "_"))
                                ),
                                UnaryExpr.Operator.LOGICAL_COMPLEMENT
                              )
                            )
                          )
                        )
                    }
                    new ConditionalExpr(
                      new BinaryExpr(new NameExpr(term.parameterName), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                      emptyOptional,
                      parameterValueExpr
                    )
                  } else {
                    val parameterValueExpr = term.emptyToNull match {
                      case EmptyIsEmpty => new NameExpr(term.parameterName)
                      case EmptyIsNull =>
                        new MethodCallExpr(
                          new MethodCallExpr(
                            new MethodCallExpr(
                              "java.util.Optional.ofNullable",
                              new NameExpr(term.parameterName)
                            ),
                            "filter",
                            new NodeList[Expression](
                              new LambdaExpr(
                                new Parameter(new UnknownType, term.parameterName + "_"),
                                new UnaryExpr(
                                  new MethodCallExpr(
                                    new StringLiteralExpr(""),
                                    "equals",
                                    new NodeList[Expression](new NameExpr(term.parameterName + "_"))
                                  ),
                                  UnaryExpr.Operator.LOGICAL_COMPLEMENT
                                )
                              )
                            )
                          ),
                          "orElse",
                          new NodeList[Expression](new NullLiteralExpr)
                        )
                    }
                    requireNonNullExpr(parameterValueExpr, Some(term.parameterName))
                  },
                  AssignExpr.Operator.ASSIGN
                )
              )
            )
      ).toNodeList
    )
  }

  private def renderMembers(
      clsName: String,
      elems: RenderedEnum[JavaLanguage]
  ) =
    Target.pure(None)

  private def encodeEnum(clsName: String, tpe: com.github.javaparser.ast.`type`.Type): Target[Option[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
    Target.pure(None)

  private def decodeEnum(clsName: String, tpe: com.github.javaparser.ast.`type`.Type): Target[Option[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
    Target.pure(None)

  private def renderClass(
      clsName: String,
      tpe: com.github.javaparser.ast.`type`.Type,
      elems: RenderedEnum[JavaLanguage]
  ) = {
    val enumType = StaticJavaParser.parseType(clsName)

    val fields = elems match {
      case RenderedStringEnum(xs) =>
        xs.map { case (value, termName, _) =>
          (termName.getIdentifier, new StringLiteralExpr(value))
        }
      case RenderedIntEnum(xs) =>
        xs.map { case (value, termName, _) =>
          (termName.getIdentifier, new IntegerLiteralExpr(value.toString()))
        }
      case RenderedLongEnum(xs) =>
        xs.map { case (value, termName, _) =>
          (termName.getIdentifier, new LongLiteralExpr(s"${value}l"))
        }
    }

    val enumDefns = fields.map { case (identifier, expr) =>
      new EnumConstantDeclaration(
        new NodeList(),
        new SimpleName(identifier),
        new NodeList(expr),
        new NodeList()
      )
    }

    val nameField = new FieldDeclaration(
      new NodeList(privateModifier, finalModifier),
      new VariableDeclarator(tpe, "name")
    )

    val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
      .addParameter(new Parameter(new NodeList(finalModifier), tpe, new SimpleName("name")))
      .setBody(
        new BlockStmt(
          new NodeList(
            new ExpressionStmt(
              new AssignExpr(
                new FieldAccessExpr(new ThisExpr, "name"),
                new NameExpr("name"),
                AssignExpr.Operator.ASSIGN
              )
            )
          )
        )
      )

    val getNameMethod = new MethodDeclaration(
      new NodeList(publicModifier),
      STRING_TYPE,
      "getName"
    ).addMarkerAnnotation("JsonValue")
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new MethodCallExpr("this.name.toString"))
          )
        )
      )

    val fromStringMethod = new MethodDeclaration(
      new NodeList(publicModifier, staticModifier),
      enumType,
      "fromString"
    ).addMarkerAnnotation("JsonCreator")
      .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")))
      .setBody(
        new BlockStmt(
          new NodeList(
            new ForEachStmt(
              new VariableDeclarationExpr(new VariableDeclarator(enumType, "value"), finalModifier),
              new MethodCallExpr("values"),
              new BlockStmt(
                new NodeList(
                  new IfStmt(
                    new MethodCallExpr("value.name.toString().equals", new NameExpr("name")),
                    new ReturnStmt(new NameExpr("value")),
                    null
                  )
                )
              )
            ),
            new ThrowStmt(
              new ObjectCreationExpr(
                null,
                StaticJavaParser.parseClassOrInterfaceType("IllegalArgumentException"),
                new NodeList(
                  new BinaryExpr(
                    new BinaryExpr(new StringLiteralExpr("Name '"), new NameExpr("name"), BinaryExpr.Operator.PLUS),
                    new StringLiteralExpr(s"' is not valid for enum '${clsName}'"),
                    BinaryExpr.Operator.PLUS
                  )
                )
              )
            )
          )
        )
      )

    val compatParseMethod = new MethodDeclaration(
      new NodeList(publicModifier, staticModifier),
      enumType,
      "parse"
    ).addMarkerAnnotation("Deprecated")
      .setJavadocComment("@deprecated See {@link #fromString(String)}")
      .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")))
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new MethodCallExpr("fromString", new NameExpr("name")))
          )
        )
      )

    val staticInitializer = new InitializerDeclaration(
      true,
      new BlockStmt(
        new NodeList(
          new ExpressionStmt(
            new MethodCallExpr(
              new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
              "register",
              new NodeList[Expression](
                new ClassExpr(StaticJavaParser.parseClassOrInterfaceType(clsName)),
                new MethodReferenceExpr(new NameExpr(clsName), null, "getName")
              )
            )
          )
        )
      )
    )

    val enumClass = new EnumDeclaration(
      new NodeList(publicModifier),
      new NodeList(),
      new SimpleName(clsName),
      new NodeList(),
      new NodeList(enumDefns: _*),
      new NodeList(
        staticInitializer,
        nameField,
        constructor,
        getNameMethod,
        fromStringMethod,
        compatParseMethod
      )
    )

    Target.pure(enumClass)
  }

  private def renderStaticDefns(
      clsName: String,
      tpe: com.github.javaparser.ast.`type`.Type,
      members: Option[Nothing],
      accessors: List[com.github.javaparser.ast.expr.Name],
      encoder: Option[com.github.javaparser.ast.body.BodyDeclaration[_ <: BodyDeclaration[_]]],
      decoder: Option[com.github.javaparser.ast.body.BodyDeclaration[_ <: BodyDeclaration[_]]]
  ): Target[StaticDefns[JavaLanguage]] =
    for {
      extraImports <- List(
        "com.fasterxml.jackson.annotation.JsonCreator",
        "com.fasterxml.jackson.annotation.JsonValue"
      ).traverse(safeParseRawImport)
    } yield StaticDefns[JavaLanguage](
      className = clsName,
      extraImports = extraImports,
      definitions = List.empty
    )

  override def buildAccessor(clsName: String, termName: String) =
    Target.pure(new Name(s"${clsName}.${termName}"))

  private def renderDTOClass(
      clsName: String,
      supportPackage: List[String],
      selfParams: List[ProtocolParameter[JavaLanguage]],
      parents: List[SuperClass[JavaLanguage]]
  ): Target[TypeDeclaration[_ <: TypeDeclaration[_]]] = {
    val parentsWithDiscriminators = parents.collect { case p if p.discriminators.nonEmpty => p }
    val discriminators            = parents.flatMap(_.discriminators)
    val discriminatorNames        = discriminators.map(_.propertyName).toSet

    def withoutDiscriminators(terms: List[ParameterTerm]): List[ParameterTerm] =
      terms.filterNot(term => discriminatorNames.contains(term.propertyName))

    def parameterGetterCall(term: ParameterTerm, scope: Option[String] = None): MethodCallExpr = {
      val methodName = getterMethodNameForParameter(term.parameterName)
      scope.fold(new MethodCallExpr(methodName))(s => new MethodCallExpr(new NameExpr(s), methodName))
    }

    def parameterToStringExpr(term: ParameterTerm, scope: Option[String] = None): Expression = term.dataRedacted match {
      case DataVisible  => parameterGetterCall(term, scope)
      case DataRedacted => new StringLiteralExpr("[redacted]")
    }

    for {
      dtoClassType <- safeParseClassOrInterfaceType(clsName)
      parentOpt <- (parentsWithDiscriminators, parents) match {
        case _ if parentsWithDiscriminators.length > 1 =>
          Target.raiseUserError[Option[SuperClass[JavaLanguage]]](
            s"${clsName} requires unsupported multiple inheritance due to multiple parents with discriminators (${parentsWithDiscriminators.map(_.clsName).mkString(", ")})"
          )
        case _ if parentsWithDiscriminators.length == 1 => Target.pure(parentsWithDiscriminators.headOption)
        case _ if parents.length == 1                   => Target.pure(parents.headOption)
        case _                                          => Target.pure(None)
      }

      parentParams                               = parentOpt.toList.flatMap(_.params)
      parentParamNames                           = parentParams.map(_.name.value)
      (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
      parentTerms                                = parentRequiredTerms ++ parentOptionalTerms

      discriminatorValues <- parentTerms
        .flatMap { term =>
          discriminators.find(_.propertyName == term.propertyName).map((term, _))
        }
        .traverse { case (term, discriminator) =>
          val discriminatorValue = discriminator.mapping
            .collectFirst { case (value, elem) if elem.name == clsName => value }
            .getOrElse(clsName)

          JacksonHelpers
            .discriminatorExpression[JavaLanguage](
              discriminator.propertyName,
              discriminatorValue,
              term.rawType
            )(
              v => Target.pure[Node](new ObjectCreationExpr(null, BIG_INTEGER_FQ_TYPE, new NodeList(new StringLiteralExpr(v)))),
              v => Target.pure[Node](new ObjectCreationExpr(null, BIG_DECIMAL_FQ_TYPE, new NodeList(new StringLiteralExpr(v)))),
              v =>
                term.fieldType match {
                  case cls: ClassOrInterfaceType =>
                    // hopefully it's an enum type; nothing else really makes sense here
                    JavaGenerator().formatEnumName(v).map(ev => new FieldAccessExpr(cls.getNameAsExpression, ev))
                  case tpe =>
                    Target.raiseUserError[Node](s"Unsupported discriminator type '${tpe.asString}' for property '${term.propertyName}'")
                }
            )(JavaGenerator())
            .flatMap[Expression] {
              case expr: Expression => Target.pure(expr)
              case node =>
                Target.raiseError(
                  RuntimeFailure(s"BUG: JacksonHelpers.discriminatorExpression() returned a ${node.getClass.getSimpleName} when we need an Expression")
                )
            }
            .map((term.propertyName, _))
        }
        .map(_.toMap)

      params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(param =>
        discriminatorNames.contains(param.term.getName.getIdentifier) || parentParamNames.contains(param.term.getName.getIdentifier)
      )
      (requiredTerms, optionalTerms) = sortParams(params)
      terms                          = requiredTerms ++ optionalTerms

      dtoClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, clsName)
        .addAnnotation(
          new NormalAnnotationExpr(
            new Name("JsonIgnoreProperties"),
            new NodeList(
              new MemberValuePair(
                "ignoreUnknown",
                new BooleanLiteralExpr(true)
              )
            )
          )
        )
        .addAnnotation(
          generatedAnnotation(this.getClass)
        )

      _ = addParents(dtoClass, parentOpt)

      _ = terms.foreach { case ParameterTerm(propertyName, parameterName, fieldType, _, _, _, _, _) =>
        val field: FieldDeclaration = dtoClass.addField(fieldType, parameterName, PRIVATE, FINAL)
        field.addSingleMemberAnnotation("JsonProperty", new StringLiteralExpr(propertyName))
      }

      primaryConstructor = dtoClass
        .addConstructor(PROTECTED)
        .addMarkerAnnotation("JsonCreator")
        .setParameters(
          new NodeList(
            withoutDiscriminators(parentTerms ++ terms).map { case ParameterTerm(propertyName, parameterName, fieldType, _, _, _, _, _) =>
              new Parameter(new NodeList(finalModifier), fieldType.box, new SimpleName(parameterName))
                .addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(propertyName)))
            }: _*
          )
        )
      superCall = new MethodCallExpr(
        "super",
        parentTerms.map(term => discriminatorValues.getOrElse(term.propertyName, new NameExpr(term.parameterName))): _*
      )
      primaryConstructorBody <- dtoConstructorBody(superCall, terms)
      _ = primaryConstructor.setBody(primaryConstructorBody)

      _ = terms.foreach(addParameterGetter(dtoClass, _))

      toStringFieldExprs = NonEmptyList
        .fromList(parentTerms ++ terms)
        .toList
        .flatMap(l =>
          (new StringLiteralExpr(s"${l.head.parameterName}="), parameterToStringExpr(l.head)) +:
            l.tail.map(term =>
              (
                new StringLiteralExpr(s", ${term.parameterName}="),
                parameterToStringExpr(term)
              )
            )
        )

      _ = dtoClass
        .addMethod("toString", PUBLIC)
        .setType(STRING_TYPE)
        .addMarkerAnnotation("Override")
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new BinaryExpr(
                  toStringFieldExprs.foldLeft[Expression](new StringLiteralExpr(s"${clsName}{")) { case (prevExpr, (strExpr, fieldExpr)) =>
                    new BinaryExpr(
                      new BinaryExpr(prevExpr, strExpr, BinaryExpr.Operator.PLUS),
                      fieldExpr,
                      BinaryExpr.Operator.PLUS
                    )
                  },
                  new StringLiteralExpr("}"),
                  BinaryExpr.Operator.PLUS
                )
              )
            )
          )
        )

      equalsConditions: List[Expression] = terms.map(term =>
        term.fieldType match {
          case _: PrimitiveType =>
            new BinaryExpr(
              parameterGetterCall(term),
              parameterGetterCall(term, Some("other")),
              BinaryExpr.Operator.EQUALS
            )
          case _ =>
            new MethodCallExpr(
              new NameExpr("java.util.Objects"),
              "equals",
              new NodeList[Expression](new FieldAccessExpr(new ThisExpr, term.parameterName), parameterGetterCall(term, Some("other")))
            )
        }
      )
      returnExpr = NonEmptyList
        .fromList(equalsConditions)
        .map(
          _.reduceLeft((prevExpr, condExpr) => new BinaryExpr(prevExpr, condExpr, BinaryExpr.Operator.AND))
        )
        .getOrElse(new BooleanLiteralExpr(true))

      _ = dtoClass
        .addMethod("equals", PUBLIC)
        .setType(PrimitiveType.booleanType)
        .addMarkerAnnotation("Override")
        .addParameter(new Parameter(new NodeList(finalModifier), OBJECT_TYPE, new SimpleName("o")))
        .setBody(
          new BlockStmt(
            new NodeList(
              new IfStmt(
                new BinaryExpr(new ThisExpr, new NameExpr("o"), BinaryExpr.Operator.EQUALS),
                new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(true)))),
                null
              ),
              new IfStmt(
                new BinaryExpr(
                  new BinaryExpr(new NameExpr("o"), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                  new BinaryExpr(new MethodCallExpr("getClass"), new MethodCallExpr(new NameExpr("o"), "getClass"), BinaryExpr.Operator.NOT_EQUALS),
                  BinaryExpr.Operator.OR
                ),
                new BlockStmt(new NodeList(new ReturnStmt(new BooleanLiteralExpr(false)))),
                null
              ),
              new ExpressionStmt(
                new VariableDeclarationExpr(
                  new VariableDeclarator(
                    dtoClassType,
                    "other",
                    new CastExpr(dtoClassType, new NameExpr("o"))
                  ),
                  finalModifier
                )
              ),
              new ReturnStmt(returnExpr)
            )
          )
        )

      _ = dtoClass
        .addMethod("hashCode", PUBLIC)
        .setType(PrimitiveType.intType)
        .addMarkerAnnotation("Override")
        .setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new MethodCallExpr(
                  new NameExpr("java.util.Objects"),
                  "hash",
                  new NodeList[Expression]((parentTerms ++ terms).map(parameterGetterCall(_, None)): _*)
                )
              )
            )
          )
        )

      builderClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, "Builder")

      _ = withoutDiscriminators(parentRequiredTerms ++ requiredTerms).foreach { case ParameterTerm(_, parameterName, fieldType, _, _, _, _, _) =>
        builderClass.addField(fieldType, parameterName, PRIVATE)
      }
      _ <- withoutDiscriminators(parentOptionalTerms ++ optionalTerms).traverse { case ParameterTerm(_, parameterName, fieldType, _, _, defaultValue, _, _) =>
        for {
          initializer <- defaultValue.fold[Target[Expression]](
            Cl.emptyOptionalTerm().flatMap(_.toExpression)
          )(dv =>
            if (fieldType.isOptionalType) {
              Cl.liftOptionalTerm(dv).flatMap(_.toExpression)
            } else {
              Target.pure(dv)
            }
          )
          _ = builderClass.addFieldWithInitializer(fieldType, parameterName, initializer, PRIVATE)
        } yield ()
      }

      _ = builderClass
        .addConstructor(PUBLIC)
        .setParameters(
          new NodeList(
            withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map { case ParameterTerm(_, parameterName, _, parameterType, _, _, _, _) =>
              new Parameter(new NodeList(finalModifier), parameterType, new SimpleName(parameterName))
            }: _*
          )
        )
        .setBody(
          new BlockStmt(
            new NodeList(
              withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map { case ParameterTerm(_, parameterName, fieldType, _, _, _, _, _) =>
                new ExpressionStmt(
                  new AssignExpr(
                    new FieldAccessExpr(new ThisExpr, parameterName),
                    fieldType match {
                      case _: PrimitiveType => new NameExpr(parameterName)
                      case _                => requireNonNullExpr(parameterName)
                    },
                    AssignExpr.Operator.ASSIGN
                  )
                )
              }: _*
            )
          )
        )

      _ = builderClass
        .addConstructor(PUBLIC)
        .setParameters(new NodeList(new Parameter(new NodeList(finalModifier), dtoClassType, new SimpleName("template"))))
        .setBody(
          new BlockStmt(
            withoutDiscriminators(parentTerms ++ terms).map { case term @ ParameterTerm(_, parameterName, _, _, _, _, _, _) =>
              new ExpressionStmt(
                new AssignExpr(
                  new FieldAccessExpr(new ThisExpr, parameterName),
                  parameterGetterCall(term, Some("template")),
                  AssignExpr.Operator.ASSIGN
                )
              ): Statement
            }.toNodeList
          )
        )

      // TODO: leave out with${name}() if readOnlyKey?
      _ <- withoutDiscriminators(parentTerms ++ terms).traverse { case ParameterTerm(_, parameterName, fieldType, parameterType, _, _, _, _) =>
        val methodName = s"with${parameterName.unescapeIdentifier.capitalize}"
        for {
          fieldInitializer <- (fieldType, parameterType) match {
            case (_: PrimitiveType, _) =>
              Target.pure[Expression](new NameExpr(parameterName))
            case (ft, pt) if ft.isOptionalType && pt.isPrimitiveType =>
              Cl.liftSomeTerm(new NameExpr(parameterName)).flatMap(_.toExpression)
            case (ft, _) if ft.isOptionalType =>
              Cl.liftOptionalTerm(new NameExpr(parameterName)).flatMap(_.toExpression)
            case _ =>
              Target.pure(requireNonNullExpr(parameterName))
          }

          _ = builderClass
            .addMethod(methodName, PUBLIC)
            .setType(BUILDER_TYPE)
            .addParameter(new Parameter(new NodeList(finalModifier), parameterType, new SimpleName(parameterName)))
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, parameterName),
                      fieldInitializer,
                      AssignExpr.Operator.ASSIGN
                    )
                  ),
                  new ReturnStmt(new ThisExpr)
                )
              )
            )

          _ =
            if (!parameterType.isOptionalType) {
              val newParameterName = s"optional${parameterName.unescapeIdentifier.capitalize}"
              for {
                newParameterType <- fieldType match {
                  case pt: PrimitiveType       => Cl.liftOptionalType(pt.toBoxedType)
                  case ft if ft.isOptionalType => Target.pure(ft)
                  case ft                      => Cl.liftOptionalType(ft)
                }
              } yield builderClass
                .addMethod(methodName, PUBLIC)
                .setType(BUILDER_TYPE)
                .addParameter(new Parameter(new NodeList(finalModifier), newParameterType, new SimpleName(newParameterName)))
                .setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(
                        if (fieldType.isOptionalType) {
                          new AssignExpr(
                            new FieldAccessExpr(new ThisExpr, parameterName),
                            requireNonNullExpr(newParameterName),
                            AssignExpr.Operator.ASSIGN
                          )
                        } else {
                          requireNonNullExpr(newParameterName)
                            .lift[Option[Any]]
                            .foreach(
                              new LambdaExpr(
                                new Parameter(new UnknownType, parameterName),
                                new AssignExpr(
                                  new FieldAccessExpr(new ThisExpr, parameterName),
                                  new NameExpr(parameterName),
                                  AssignExpr.Operator.ASSIGN
                                )
                              ).lift[Any => Unit]
                            )
                            .value
                        }
                      ),
                      new ReturnStmt(new ThisExpr)
                    )
                  )
                )
            } else {
              ()
            }
        } yield ()
      }

      builderBuildTerms = withoutDiscriminators(parentTerms ++ terms)
      _ = builderClass
        .addMethod("build", PUBLIC)
        .setType(clsName)
        .setBody(
          new BlockStmt(
            (
              builderBuildTerms
                .filterNot(_.fieldType.isPrimitiveType)
                .map(term => new ExpressionStmt(requireNonNullExpr(new FieldAccessExpr(new ThisExpr, term.parameterName)))) :+ new ReturnStmt(
                new ObjectCreationExpr(
                  null,
                  StaticJavaParser.parseClassOrInterfaceType(clsName),
                  new NodeList(
                    builderBuildTerms.map(param => new FieldAccessExpr(new ThisExpr, param.parameterName)): _*
                  )
                )
              )
            ).toNodeList
          )
        )

      _ = dtoClass.addMember(builderClass)

    } yield dtoClass
  }

  private def extractProperties(spec: Tracker[Schema[_]]) =
    spec
      .refine[Target[List[(String, Tracker[Schema[_]])]]] { case m: ObjectSchema => m }(m =>
        Target.pure(m.downField("properties", _.getProperties()).indexedCosequence.value)
      )
      .orRefine { case c: ComposedSchema => c } { comp =>
        val extractedProps =
          comp
            .downField("allOf", _.getAllOf())
            .indexedDistribute
            .lastOption
            .toList
            .flatMap(_.downField("properties", _.getProperties).indexedCosequence.value.toList)
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
      concreteTypes: List[PropMeta[JavaLanguage]]
  )(
      name: String,
      fieldName: String,
      property: Tracker[Schema[_]],
      meta: core.ResolvedType[JavaLanguage],
      requirement: PropertyRequirement,
      isCustomType: Boolean,
      defaultValue: Option[com.github.javaparser.ast.Node]
  ) =
    Target.log.function("transformProperty") {
      val fallbackRawType = LiteralRawType(property.downField("type", _.getType()).unwrapTracker, property.downField("format", _.getFormat()).unwrapTracker)
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
          case core.Resolved(declType, classDep, _, rawType) =>
            Target.pure((declType, classDep, rawType))
          case core.Deferred(tpeName) =>
            val tpe = concreteTypes.find(_.clsName == tpeName).map(x => Target.pure(x.tpe)).getOrElse {
              println(s"Unable to find definition for ${tpeName}, just inlining")
              safeParseType(tpeName)
            }
            tpe.map((_, Option.empty, fallbackRawType))
          case core.DeferredArray(tpeName, containerTpe) =>
            val concreteType = lookupTypeName(tpeName, concreteTypes)
            for {
              innerType <- concreteType.fold(safeParseType(tpeName))(Target.pure)
              tpe       <- Cl.liftVectorType(innerType, containerTpe)
            } yield (tpe, Option.empty, ReifiedRawType.ofVector(fallbackRawType))
          case core.DeferredMap(tpeName, containerTpe) =>
            val concreteType = lookupTypeName(tpeName, concreteTypes)
            for {
              innerType <- concreteType.fold(safeParseType(tpeName))(Target.pure)
              tpe       <- Cl.liftMapType(innerType, containerTpe)
            } yield (tpe, Option.empty, ReifiedRawType.ofVector(fallbackRawType))
        }

        expressionDefaultValue <- (defaultValue match {
          case Some(e: Expression) => Target.pure(Some(e))
          case Some(_) =>
            Target.log.warning(s"Can't generate default value for class $clsName and property $name.") >> Target.pure(None)
          case None => Target.pure(None)
        }): Target[Option[Expression]]
        finalDefaultTypeValue <- Option(requirement)
          .filter {
            case PropertyRequirement.Required => true
            case _                            => false
          }
          .fold[Target[(Type, Option[Expression])]](
            for {
              optionalTpe      <- Cl.liftOptionalType(tpe)
              defaultValueExpr <- defaultValue.fold(Target.pure(Option.empty[Expression]))(dv => dv.toExpression.map(Option.apply))
            } yield (optionalTpe, defaultValueExpr)
          )(Function.const(Target.pure((tpe, expressionDefaultValue))) _)
        (finalDeclType, finalDefaultValue) = finalDefaultTypeValue
        term <- safeParseParameter(s"final ${finalDeclType} $fieldName")
        dep     = classDep.filterNot(_.asString == clsName) // Filter out our own class name
        pattern = property.downField("pattern", _.getPattern).map(PropertyValidations)
      } yield ProtocolParameter[JavaLanguage](
        term,
        finalDeclType,
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

  private def encodeModel(
      clsName: String,
      dtoPackage: List[String],
      selfParams: List[ProtocolParameter[JavaLanguage]],
      parents: List[SuperClass[JavaLanguage]] = Nil
  ) =
    Target.pure(None)

  private def decodeModel(
      clsName: String,
      dtoPackage: List[String],
      supportPackage: List[String],
      selfParams: List[ProtocolParameter[JavaLanguage]],
      parents: List[SuperClass[JavaLanguage]] = Nil
  ) =
    Target.pure(None)

  private def renderDTOStaticDefns(
      clsName: String,
      deps: List[com.github.javaparser.ast.expr.Name],
      encoder: Option[com.github.javaparser.ast.body.VariableDeclarator],
      decoder: Option[com.github.javaparser.ast.body.VariableDeclarator],
      protocolParameters: List[ProtocolParameter[JavaLanguage]]
  ) =
    Target.pure(StaticDefns[JavaLanguage](clsName, List.empty, List.empty))

  private def extractArrayType(
      arr: core.ResolvedType[JavaLanguage],
      concreteTypes: List[PropMeta[JavaLanguage]]
  ): Target[Type] =
    for {
      result <- arr match {
        case core.Resolved(tpe, dep, default, _) => Target.pure(tpe)
        case core.Deferred(tpeName) =>
          Target.fromOption(lookupTypeName(tpeName, concreteTypes), UserError(s"Unresolved reference ${tpeName}"))
        case core.DeferredArray(tpeName, containerTpe) =>
          lookupTypeName(tpeName, concreteTypes)
            .fold[Target[Type]](Target.raiseUserError(s"Unresolved reference ${tpeName}"))(Cl.liftVectorType(_, containerTpe))
        case core.DeferredMap(tpeName, containerTpe) =>
          lookupTypeName(tpeName, concreteTypes).fold[Target[Type]](
            Target.raiseUserError(s"Unresolved reference ${tpeName}")
          )(tpe => Cl.liftMapType(tpe, None).flatMap(mapTpe => Cl.liftVectorType(mapTpe, containerTpe)))
      }
    } yield result

  private def extractConcreteTypes(definitions: Either[String, List[PropMeta[JavaLanguage]]]) =
    definitions.fold[Target[List[PropMeta[JavaLanguage]]]](Target.raiseUserError, Target.pure)

  private def protocolImports() =
    (List(
      "com.fasterxml.jackson.annotation.JsonCreator",
      "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
      "com.fasterxml.jackson.annotation.JsonProperty",
      "com.fasterxml.jackson.annotation.JsonValue"
    ).map(safeParseRawImport) ++ List(
      "java.util.Objects.requireNonNull"
    ).map(safeParseRawStaticImport)).sequence

  override def staticProtocolImports(pkgName: List[String]) =
    Target.pure(List.empty)

  override def generateSupportDefinitions() =
    Target.pure(List.empty)

  private def packageObjectImports() =
    Target.pure(List.empty)

  private def packageObjectContents() =
    Target.pure(List.empty)

  private def implicitsObject() =
    Target.pure(None)

  private def renderSealedTrait(
      className: String,
      selfParams: List[ProtocolParameter[JavaLanguage]],
      discriminator: Discriminator[JavaLanguage],
      parents: List[SuperClass[JavaLanguage]],
      children: List[String]
  ) = {
    val parentsWithDiscriminators = parents.collect { case p if p.discriminators.nonEmpty => p }
    for {
      parentOpt <- (parentsWithDiscriminators, parents) match {
        case _ if parentsWithDiscriminators.length > 1 =>
          Target.raiseUserError[Option[SuperClass[JavaLanguage]]](
            s"${className} requires unsupported multiple inheritance due to multiple parents with discriminators (${parentsWithDiscriminators.map(_.clsName).mkString(", ")})"
          )
        case _ if parentsWithDiscriminators.length == 1 => Target.pure(parentsWithDiscriminators.headOption)
        case _ if parents.length == 1                   => Target.pure(parents.headOption)
        case _                                          => Target.pure(None)
      }

      parentParams                               = parentOpt.toList.flatMap(_.params)
      parentParamNames                           = parentParams.map(_.name.value)
      (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
      parentTerms                                = parentRequiredTerms ++ parentOptionalTerms
      params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(param =>
        parentParamNames.contains(param.term.getName.getIdentifier)
      )
      (requiredTerms, optionalTerms) = sortParams(params)
      terms                          = requiredTerms ++ optionalTerms

      abstractClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, className)
        .addAnnotation(
          new NormalAnnotationExpr(
            new Name("JsonIgnoreProperties"),
            new NodeList(
              new MemberValuePair(
                "ignoreUnknown",
                new BooleanLiteralExpr(true)
              )
            )
          )
        )
        .addAnnotation(
          new NormalAnnotationExpr(
            new Name("JsonTypeInfo"),
            new NodeList(
              new MemberValuePair(
                "use",
                new FieldAccessExpr(new NameExpr("JsonTypeInfo.Id"), "NAME")
              ),
              new MemberValuePair(
                "include",
                new FieldAccessExpr(new NameExpr("JsonTypeInfo.As"), "PROPERTY")
              ),
              new MemberValuePair(
                "property",
                new StringLiteralExpr(discriminator.propertyName)
              )
            )
          )
        )
        .addSingleMemberAnnotation(
          "JsonSubTypes",
          new ArrayInitializerExpr(
            new NodeList(
              children.map(child =>
                new NormalAnnotationExpr(
                  new Name("JsonSubTypes.Type"),
                  new NodeList(
                    new MemberValuePair(
                      "name",
                      new StringLiteralExpr(
                        discriminator.mapping
                          .collectFirst { case (value, elem) if elem.name == child => value }
                          .getOrElse(child)
                      )
                    ),
                    new MemberValuePair("value", new ClassExpr(StaticJavaParser.parseType(child)))
                  )
                )
              ): _*
            )
          )
        )

      _ = addParents(abstractClass, parentOpt)

      _ = terms.foreach { term =>
        val field = abstractClass.addField(term.fieldType, term.parameterName, PRIVATE, FINAL)
        field.addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(term.propertyName)))
      }

      superCall = new MethodCallExpr("super", parentTerms.map(term => new NameExpr(term.parameterName)): _*)
      constructorBody <- dtoConstructorBody(superCall, requiredTerms ++ optionalTerms)
      _ = abstractClass
        .addConstructor(PROTECTED)
        .setParameters(
          (parentTerms ++ terms)
            .map(term => new Parameter(new NodeList(finalModifier), term.fieldType.box, new SimpleName(term.parameterName)))
            .toNodeList
        )
        .setBody(constructorBody)

      _ = terms.foreach(addParameterGetter(abstractClass, _))
    } yield abstractClass
  }

  private def extractSuperClass(
      spec: Tracker[ComposedSchema],
      definitions: List[(String, Tracker[Schema[_]])]
  ) = {
    def allParents(model: Tracker[Schema[_]]): List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])] =
      model
        .refine { case c: ComposedSchema => c }(
          _.downField("allOf", _.getAllOf()).indexedDistribute
            .flatMap { elem =>
              definitions
                .collectFirst {
                  case (clsName, e) if elem.downField("$ref", _.get$ref()).exists(_.unwrapTracker.endsWith(s"/$clsName")) =>
                    (clsName, e, List.empty) :: allParents(e)
                }
                .getOrElse(List.empty)
            }
        )
        .getOrElse(List.empty)

    Target.pure(allParents(spec))
  }

  private def renderADTStaticDefns(
      clsName: String,
      discriminator: Discriminator[JavaLanguage],
      encoder: Option[com.github.javaparser.ast.body.VariableDeclarator],
      decoder: Option[com.github.javaparser.ast.body.VariableDeclarator]
  ) =
    for {
      extraImports <- List(
        "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
        "com.fasterxml.jackson.annotation.JsonSubTypes",
        "com.fasterxml.jackson.annotation.JsonTypeInfo"
      ).traverse(safeParseRawImport)
    } yield StaticDefns[JavaLanguage](
      clsName,
      extraImports,
      List.empty
    )

  private def decodeADT(
      clsName: String,
      discriminator: Discriminator[JavaLanguage],
      children: List[String] = Nil
  ) =
    Target.pure(None)

  private def encodeADT(
      clsName: String,
      discriminator: Discriminator[JavaLanguage],
      children: List[String] = Nil
  ) =
    Target.pure(None)
}
