package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models._
import _root_.io.swagger.v3.oas.models.media._
import cats.data.NonEmptyList
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.SwaggerUtil.Resolved
import com.twilio.guardrail.extract.VendorExtension.VendorExtensible._
import com.twilio.guardrail.generators.RawParameterType
import com.twilio.guardrail.generators.syntax._
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import java.util.Locale

import com.twilio.guardrail.extract.Default

import scala.collection.JavaConverters._
import scala.language.higherKinds

case class ProtocolDefinitions[L <: LA](elems: List[StrictProtocolElems[L]],
                                        protocolImports: List[L#Import],
                                        packageObjectImports: List[L#Import],
                                        packageObjectContents: List[L#ValueDefinition])
sealed trait EmptyToNullBehaviour
case object EmptyIsNull  extends EmptyToNullBehaviour
case object EmptyIsEmpty extends EmptyToNullBehaviour

sealed trait RedactionBehaviour
case object DataVisible  extends RedactionBehaviour
case object DataRedacted extends RedactionBehaviour

case class ProtocolParameter[L <: LA](term: L#MethodParameter,
                                      name: String,
                                      dep: Option[L#TermName],
                                      rawType: RawParameterType,
                                      readOnlyKey: Option[String],
                                      emptyToNull: EmptyToNullBehaviour,
                                      dataRedaction: RedactionBehaviour,
                                      defaultValue: Option[L#Term])

case class Discriminator[L <: LA](propertyName: String, mapping: Map[String, ProtocolElems[L]])

object Discriminator {
  def fromSchema[L <: LA, F[_]](schema: Schema[_])(implicit Sc: ScalaTerms[L, F]): Free[F, Option[Discriminator[L]]] = {
    import Sc._
    Option(schema.getDiscriminator)
      .flatMap(x => Option(x.getPropertyName).map((x, _)))
      .traverse {
        case (x, propertyName) =>
          val possibleMappings = Option(x.getMapping)
            .map(_.asScala)
            .getOrElse(Map.empty[String, String])
            .flatMap({
              case (k, s) if s.startsWith("#/") => s.split("/").lastOption.filter(_.nonEmpty).map((k, _))
              case (k, s)                       => Option((k, s))
            })
            .toList
          for {
            mapping <- possibleMappings.flatTraverse({
              case (key, name) =>
                parseType(name).map(_.map(tpe => (key, RandomType(name, tpe))).toList)
            })
          } yield Discriminator[L](propertyName, mapping.toMap)
      }
  }
}

case class SuperClass[L <: LA](
    clsName: String,
    tpl: L#TypeName,
    interfaces: List[String],
    params: List[ProtocolParameter[L]],
    discriminators: List[Discriminator[L]]
)

object ProtocolGenerator {
  private[this] def getRequiredFieldsRec(root: Schema[_]): List[String] = {
    @scala.annotation.tailrec
    def work(values: List[Schema[_]], acc: List[String]): List[String] = {
      val required = values.flatMap(value => Option(value.getRequired()).fold(List.empty[String])(_.asScala.toList))
      val next: List[Schema[_]] = values.flatMap({
        case x: ComposedSchema =>
          Option(x.getAllOf())
            .fold(List.empty[Schema[_]])(_.asScala.toList)
        case _ => Nil
      })

      val newRequired = acc ++ required

      next match {
        case next @ (_ :: _) => work(next, newRequired)
        case Nil             => newRequired
      }
    }
    work(List(root), Nil)
  }

  private[this] def fromEnum[L <: LA, F[_]](
      clsName: String,
      swagger: Schema[_]
  )(implicit E: EnumProtocolTerms[L, F], F: FrameworkTerms[L, F], Sc: ScalaTerms[L, F], Sw: SwaggerTerms[L, F]): Free[F, Either[String, EnumDefinition[L]]] = {
    import E._
    import Sc._

    def validProg(enum: List[String], tpe: L#Type): Free[F, EnumDefinition[L]] =
      for {
        elems <- enum.traverse { elem =>
          for {
            termName  <- formatEnumName(elem)
            valueTerm <- pureTermName(termName)
            accessor  <- buildAccessor(clsName, termName)
          } yield (elem, valueTerm, accessor)
        }
        pascalValues = elems.map(_._2)
        members <- renderMembers(clsName, elems)
        encoder <- encodeEnum(clsName)
        decoder <- decodeEnum(clsName)

        defn        <- renderClass(clsName, tpe, elems)
        staticDefns <- renderStaticDefns(clsName, members, pascalValues, encoder, decoder)
        classType   <- pureTypeName(clsName)
      } yield EnumDefinition[L](clsName, classType, elems, defn, staticDefns)

    // Default to `string` for untyped enums.
    // Currently, only plain strings are correctly supported anyway, so no big loss.
    val tpeName = Option(swagger.getType).filterNot(_ == "object").getOrElse("string")

    for {
      enum          <- extractEnum(swagger)
      customTpeName <- SwaggerUtil.customTypeName(swagger)
      tpe           <- SwaggerUtil.typeName(tpeName, Option(swagger.getFormat()), customTpeName)
      res           <- enum.traverse(validProg(_, tpe))
    } yield res
  }

  /**
    * types of things we can losslessly convert between snake and camel case:
    *   - foo
    *   - foo_bar
    *   - foo_bar_baz
    *   - foo.bar
    *
    * types of things we canNOT losslessly convert between snake and camel case:
    *   - Foo
    *   - Foo_bar
    *   - Foo_Bar
    *   - FooBar
    *   - foo_barBaz
    *
    * so essentially we have to return false if:
    *   - there are any uppercase characters
    */
  def couldBeSnakeCase(s: String): Boolean = s.toLowerCase(Locale.US) == s

  /**
    * Handle polymorphic model
    */
  private[this] def fromPoly[L <: LA, F[_]](
      hierarchy: ClassParent[L],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Schema[_])]
  )(implicit F: FrameworkTerms[L, F],
    P: PolyProtocolTerms[L, F],
    E: EnumProtocolTerms[L, F],
    M: ModelProtocolTerms[L, F],
    Sc: ScalaTerms[L, F],
    Sw: SwaggerTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import M._
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
      parents <- hierarchy.model match {
        case c: ComposedSchema => extractParents(c, definitions, concreteTypes)
        case _                 => Free.pure[F, List[SuperClass[L]]](Nil)
      }
      props <- extractProperties(hierarchy.model)
      requiredFields           = hierarchy.required ::: hierarchy.children.flatMap(_.required)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse({
        case (name, prop) =>
          val typeName   = NonEmptyList.of(hierarchy.name, name.toCamelCase.capitalize)
          val isRequired = requiredFields.contains(name)
          for {
            customType   <- SwaggerUtil.customTypeName(prop)
            resolvedType <- SwaggerUtil.propMeta[L, F](prop)
            defValue     <- defaultValue(typeName, prop, isRequired, definitions)
            res <- transformProperty(hierarchy.name, needCamelSnakeConversion, concreteTypes)(name,
                                                                                              prop,
                                                                                              resolvedType,
                                                                                              isRequired,
                                                                                              customType.isDefined,
                                                                                              defValue)
          } yield res
      })
      definition  <- renderSealedTrait(hierarchy.name, params, discriminator, parents, children)
      encoder     <- encodeADT(hierarchy.name, hierarchy.discriminator, children)
      decoder     <- decodeADT(hierarchy.name, hierarchy.discriminator, children)
      staticDefns <- renderADTStaticDefns(hierarchy.name, discriminator, encoder, decoder)
      tpe         <- pureTypeName(hierarchy.name)
    } yield {
      ADT[L](
        name = hierarchy.name,
        tpe = tpe,
        trt = definition,
        staticDefns = staticDefns
      )
    }
  }

  def extractParents[L <: LA, F[_]](elem: ComposedSchema, definitions: List[(String, Schema[_])], concreteTypes: List[PropMeta[L]])(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      E: EnumProtocolTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, List[SuperClass[L]]] = {
    import M._
    import P._
    import Sc._

    for {
      a <- extractSuperClass(elem, definitions)
      supper <- a.flatTraverse {
        case (clsName, _extends, interfaces) =>
          val concreteInterfaces = interfaces
            .flatMap(
              x =>
                definitions.collectFirst[Schema[_]] {
                  case (cls, y: ComposedSchema) if Option(x.get$ref).exists(_.endsWith(s"/${cls}")) => y
                  case (cls, y: Schema[_]) if Option(x.get$ref).exists(_.endsWith(s"/${cls}"))      => y
              }
            )
          for {
            _extendsProps <- extractProperties(_extends)
            requiredFields = getRequiredFieldsRec(_extends) ++ concreteInterfaces.flatMap(getRequiredFieldsRec)
            _withProps <- concreteInterfaces.traverse(extractProperties)
            props = _extendsProps ++ _withProps.flatten
            (params, _) <- prepareProperties(NonEmptyList.of(clsName), props, requiredFields, concreteTypes, definitions)
            interfacesCls = interfaces.flatMap(i => Option(i.get$ref).map(_.split("/").last))
            tpe <- parseTypeName(clsName)

            discriminators <- (_extends :: concreteInterfaces).flatTraverse({
              case m: ObjectSchema => Discriminator.fromSchema(m).map(_.toList)
              case _               => Free.pure[F, List[Discriminator[L]]](List.empty)
            })
          } yield
            tpe
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

  private[this] def fromModel[L <: LA, F[_]](clsName: NonEmptyList[String],
                                             model: Schema[_],
                                             parents: List[SuperClass[L]],
                                             concreteTypes: List[PropMeta[L]],
                                             definitions: List[(String, Schema[_])])(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      E: EnumProtocolTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Either[String, ClassDefinition[L]]] = {
    import M._
    import Sc._

    for {
      props <- extractProperties(model)
      requiredFields           = getRequiredFieldsRec(model)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      (params, nestedDefinitions) <- prepareProperties(clsName, props, requiredFields, concreteTypes, definitions)
      defn                        <- renderDTOClass(clsName.last, params, parents)
      encoder                     <- encodeModel(clsName.last, needCamelSnakeConversion, params, parents)
      decoder                     <- decodeModel(clsName.last, needCamelSnakeConversion, params, parents)
      tpe                         <- parseTypeName(clsName.last)
      staticDefns                 <- renderDTOStaticDefns(clsName.last, List.empty, encoder, decoder)
      result <- if (parents.isEmpty && props.isEmpty) Free.pure[F, Either[String, ClassDefinition[L]]](Left("Entity isn't model"))
      else {
        val nestedClasses = nestedDefinitions.flatTraverse {
          case classDefinition: ClassDefinition[L] =>
            for {
              widenClass          <- widenClassDefinition(classDefinition.cls)
              companionTerm       <- pureTermName(classDefinition.name)
              companionDefinition <- wrapToObject(companionTerm, classDefinition.staticDefns.extraImports, classDefinition.staticDefns.definitions)
              widenCompanion      <- widenObjectDefinition(companionDefinition)
            } yield List(widenClass, widenCompanion)
          case enumDefinition: EnumDefinition[L] =>
            for {
              widenClass          <- widenClassDefinition(enumDefinition.cls)
              companionTerm       <- pureTermName(enumDefinition.name)
              companionDefinition <- wrapToObject(companionTerm, enumDefinition.staticDefns.extraImports, enumDefinition.staticDefns.definitions)
              widenCompanion      <- widenObjectDefinition(companionDefinition)
            } yield List(widenClass, widenCompanion)
        }
        nestedClasses.map { v =>
          val finalStaticDefns = staticDefns.copy(definitions = staticDefns.definitions ++ v)
          tpe.toRight("Empty entity name").map(ClassDefinition[L](clsName.last, _, defn, finalStaticDefns, parents))
        }
      }
    } yield result
  }

  private def prepareProperties[L <: LA, F[_]](
      clsName: NonEmptyList[String],
      props: List[(String, Schema[_])],
      requiredFields: List[String],
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Schema[_])]
  )(implicit M: ModelProtocolTerms[L, F],
    F: FrameworkTerms[L, F],
    E: EnumProtocolTerms[L, F],
    P: PolyProtocolTerms[L, F],
    Sc: ScalaTerms[L, F],
    Sw: SwaggerTerms[L, F]): Free[F, (List[ProtocolParameter[L]], List[NestedProtocolElems[L]])] = {
    import F._
    import M._
    import Sc._
    def processProperty(name: String, schema: Schema[_]): Free[F, Option[Either[String, NestedProtocolElems[L]]]] = {
      val nestedClassName = clsName.append(name.toCamelCase.capitalize)
      schema match {
        case _: ObjectSchema =>
          fromModel(nestedClassName, schema, List.empty, concreteTypes, definitions).map(Some(_))
        case o: ComposedSchema =>
          for {
            parents              <- extractParents(o, definitions, concreteTypes)
            maybeClassDefinition <- fromModel(nestedClassName, schema, parents, concreteTypes, definitions)
          } yield Some(maybeClassDefinition)
        case a: ArraySchema =>
          processProperty(name, a.getItems)
        case s: StringSchema if Option(s.getEnum).map(_.asScala).exists(_.nonEmpty) =>
          fromEnum(nestedClassName.last, s).map(Some(_))
        case _ =>
          Free.pure(None)
      }
    }
    val needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
    for {
      paramsAndNestedDefinitions <- props.traverse[Free[F, ?], (ProtocolParameter[L], Option[NestedProtocolElems[L]])] {
        case (name, schema) =>
          val typeName = clsName.append(name.toCamelCase.capitalize)
          for {
            tpe                   <- selectType(typeName)
            maybeNestedDefinition <- processProperty(name, schema)
            resolvedType <- maybeNestedDefinition.fold(SwaggerUtil.propMetaWithName(tpe, schema)) {
              case Left(_)  => objectType(None).map(Resolved(_, None, None, None, None))
              case Right(_) => SwaggerUtil.propMetaWithName(tpe, schema)
            }
            customType <- SwaggerUtil.customTypeName(schema)
            isRequired = requiredFields.contains(name)
            defValue <- defaultValue(typeName, schema, isRequired, definitions)
            parameter <- transformProperty(clsName.last, needCamelSnakeConversion, concreteTypes)(name,
                                                                                                  schema,
                                                                                                  resolvedType,
                                                                                                  isRequired,
                                                                                                  customType.isDefined,
                                                                                                  defValue)
          } yield (parameter, maybeNestedDefinition.flatMap(_.toOption))
      }
      (params, nestedDefinitions) = paramsAndNestedDefinitions.unzip
    } yield params -> nestedDefinitions.flatten
  }

  def modelTypeAlias[L <: LA, F[_]](clsName: String, abstractModel: Schema[_])(
      implicit
      Fw: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import Fw._
    val model = abstractModel match {
      case m: ObjectSchema => Some(m)
      case m: ComposedSchema =>
        m.getAllOf.asScala.toList.get(1).flatMap {
          case m: ObjectSchema => Some(m)
          case _               => None
        }
      case _ => None
    }
    for {
      tpe <- model
        .flatMap(model => Option(model.getType))
        .fold[Free[F, L#Type]](objectType(None))(
          raw =>
            model
              .flatTraverse(SwaggerUtil.customTypeName[L, F, ObjectSchema])
              .flatMap(customTypeName => SwaggerUtil.typeName[L, F](raw, model.flatMap(f => Option(f.getFormat)), customTypeName))
        )
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def plainTypeAlias[L <: LA, F[_]](
      clsName: String
  )(implicit Fw: FrameworkTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import Fw._
    for {
      tpe <- objectType(None)
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def typeAlias[L <: LA, F[_]](clsName: String, tpe: L#Type): Free[F, ProtocolElems[L]] =
    Free.pure(RandomType[L](clsName, tpe))

  def fromArray[L <: LA, F[_]](clsName: String, arr: ArraySchema, concreteTypes: List[PropMeta[L]])(
      implicit R: ArrayProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      P: ProtocolSupportTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import R._
    for {
      deferredTpe <- SwaggerUtil.modelMetaType(arr)
      tpe         <- extractArrayType(deferredTpe, concreteTypes)
      ret         <- typeAlias[L, F](clsName, tpe)
    } yield ret
  }

  sealed trait ClassHierarchy[L <: LA] {
    def name: String
    def model: Schema[_]
    def children: List[ClassChild[L]]
    def required: List[String]
  }
  case class ClassChild[L <: LA](name: String, model: Schema[_], children: List[ClassChild[L]], required: List[String]) extends ClassHierarchy[L]
  case class ClassParent[L <: LA](name: String, model: Schema[_], children: List[ClassChild[L]], discriminator: Discriminator[L], required: List[String])
      extends ClassHierarchy[L]

  /**
    * returns objects grouped into hierarchies
    */
  def groupHierarchies[L <: LA, F[_]](
      definitions: List[(String, Schema[_])]
  )(implicit Sc: ScalaTerms[L, F]): Free[F, (List[ClassParent[L]], List[(String, Schema[_])])] = {

    def firstInHierarchy(model: Schema[_]): Option[ObjectSchema] =
      (model match {
        case elem: ComposedSchema =>
          definitions.collectFirst {
            case (clsName, element) if Option(elem.getAllOf).toList.flatMap(_.asScala).exists(r => Option(r.get$ref).exists(_.endsWith(s"/$clsName"))) =>
              element
          }
        case _ => None
      }) match {
        case Some(x: ComposedSchema) => firstInHierarchy(x)
        case Some(x: ObjectSchema)   => Some(x)
        case _                       => None
      }

    def children(cls: String): List[ClassChild[L]] = definitions.collect {
      case (clsName, comp: ComposedSchema)
          if Option(comp.getAllOf)
            .map(_.asScala)
            .getOrElse(List.empty)
            .exists(x => Option(x.get$ref).exists(_.endsWith(s"/$cls"))) =>
        ClassChild(clsName, comp, children(clsName), getRequiredFieldsRec(comp))
    }

    def classHierarchy(cls: String, model: Schema[_]): Free[F, Option[ClassParent[L]]] =
      (model match {
        case c: ComposedSchema =>
          firstInHierarchy(c)
            .fold(Free.pure[F, Option[Discriminator[L]]](Option.empty))(Discriminator.fromSchema)
            .map(_.map((_, getRequiredFieldsRec(c))))
        case m: Schema[_]                            => Discriminator.fromSchema(m).map(_.map((_, getRequiredFieldsRec(m))))
        case _                                       => Free.pure[F, Option[(Discriminator[L], List[String])]](Option.empty)
      }).map(_.map({ case (discriminator, reqFields) => ClassParent(cls, model, children(cls), discriminator, reqFields) }))

    definitions
      .traverse({
        case (cls, model) =>
          for {
            hierarchy <- classHierarchy(cls, model)
          } yield hierarchy.filterNot(_.children.isEmpty).toLeft((cls, model))
      })
      .map(_.partitionEither[ClassParent[L], (String, Schema[_])](identity))
  }

  def fromSwagger[L <: LA, F[_]](swagger: OpenAPI)(
      implicit E: EnumProtocolTerms[L, F],
      M: ModelProtocolTerms[L, F],
      R: ArrayProtocolTerms[L, F],
      S: ProtocolSupportTerms[L, F],
      F: FrameworkTerms[L, F],
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolDefinitions[L]] = {
    import S._
    import Sw._

    val definitions = Option(swagger.getComponents()).toList.flatMap(x => Option(x.getSchemas)).flatMap(_.asScala.toList)

    for {
      (hierarchies, definitionsWithoutPoly) <- groupHierarchies(definitions)

      concreteTypes <- SwaggerUtil.extractConcreteTypes[L, F](definitions)
      polyADTs      <- hierarchies.traverse(fromPoly(_, concreteTypes, definitions))
      elems <- definitionsWithoutPoly.traverse {
        case (clsName, model) =>
          model match {
            case m: StringSchema =>
              for {
                enum  <- fromEnum(clsName, m)
                model <- fromModel(NonEmptyList.of(clsName), m, List.empty, concreteTypes, definitions)
                alias <- modelTypeAlias(clsName, m)
              } yield enum.orElse(model).getOrElse(alias)

            case comp: ComposedSchema =>
              for {
                parents <- extractParents(comp, definitions, concreteTypes)
                model   <- fromModel(NonEmptyList.of(clsName), comp, parents, concreteTypes, definitions)
                alias   <- modelTypeAlias(clsName, comp)
              } yield model.getOrElse(alias)

            case arr: ArraySchema =>
              fromArray(clsName, arr, concreteTypes)

            case m: ObjectSchema =>
              for {
                enum  <- fromEnum(clsName, m)
                model <- fromModel(NonEmptyList.of(clsName), m, List.empty, concreteTypes, definitions)
                alias <- modelTypeAlias(clsName, m)
              } yield enum.orElse(model).getOrElse(alias)

            case x =>
              for {
                tpeName        <- getType(x)
                customTypeName <- SwaggerUtil.customTypeName(x)
                tpe            <- SwaggerUtil.typeName[L, F](tpeName, Option(x.getFormat()), customTypeName)
                res            <- typeAlias(clsName, tpe)
              } yield res
          }
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()

      polyADTElems <- ProtocolElems.resolve[L, F](polyADTs)
      strictElems  <- ProtocolElems.resolve[L, F](elems)
    } yield ProtocolDefinitions[L](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents)
  }

  private def defaultValue[L <: LA, F[_]](name: NonEmptyList[String], schema: Schema[_], isRequired: Boolean, definitions: List[(String, Schema[_])])(
      implicit Sc: ScalaTerms[L, F]
  ): Free[F, Option[L#Term]] = {
    import Sc._
    val empty = Free.pure[F, Option[L#Term]](None)
    Option(schema.get$ref()) match {
      case Some(ref) =>
        definitions
          .collectFirst {
            case (cls, refSchema) if ref.endsWith(s"/$cls") =>
              defaultValue(NonEmptyList.of(cls), refSchema, isRequired, definitions)
          }
          .getOrElse(Free.pure(None))
      case None =>
        schema match {
          case _: MapSchema if isRequired =>
            emptyMap.map(Some(_))
          case _: ArraySchema if isRequired =>
            emptyArray.map(Some(_))
          case p: BooleanSchema =>
            Default(p).extract[Boolean].fold(empty)(litBoolean(_).map(Some(_)))
          case p: NumberSchema if p.getFormat == "double" =>
            Default(p).extract[Double].fold(empty)(litDouble(_).map(Some(_)))
          case p: NumberSchema if p.getFormat == "float" =>
            Default(p).extract[Float].fold(empty)(litFloat(_).map(Some(_)))
          case p: IntegerSchema if p.getFormat == "int32" =>
            Default(p).extract[Int].fold(empty)(litInt(_).map(Some(_)))
          case p: IntegerSchema if p.getFormat == "int64" =>
            Default(p).extract[Long].fold(empty)(litLong(_).map(Some(_)))
          case p: StringSchema if Option(p.getEnum).map(_.asScala).exists(_.nonEmpty) =>
            Default(p).extract[String] match {
              case Some(defaultEnumValue) =>
                for {
                  enumName <- formatEnumName(defaultEnumValue)
                  result   <- selectTerm(name.append(enumName))
                } yield Some(result)
              case None => Free.pure(None)
            }
          case p: StringSchema =>
            Default(p).extract[String].fold(empty)(litString(_).map(Some(_)))
          case _ =>
            Free.pure(None)
        }
    }

  }
}
