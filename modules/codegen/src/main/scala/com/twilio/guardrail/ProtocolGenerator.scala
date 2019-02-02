package com.twilio.guardrail

import _root_.io.swagger.v3.oas.models._
import _root_.io.swagger.v3.oas.models.media.{ ArraySchema, ComposedSchema, ObjectSchema, Schema, StringSchema, IntegerSchema }
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.extract.ScalaType
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }
import java.util.Locale
import scala.collection.JavaConverters._
import scala.language.{ higherKinds, postfixOps, reflectiveCalls }

case class ProtocolDefinitions[L <: LA](elems: List[StrictProtocolElems[L]],
                                        protocolImports: List[L#Import],
                                        packageObjectImports: List[L#Import],
                                        packageObjectContents: List[L#ValueDefinition])
sealed trait EmptyToNullBehaviour
case object EmptyIsNull  extends EmptyToNullBehaviour
case object EmptyIsEmpty extends EmptyToNullBehaviour

case class ProtocolParameter[L <: LA](term: L#MethodParameter,
                                      name: String,
                                      dep: Option[L#TermName],
                                      readOnlyKey: Option[String],
                                      emptyToNull: EmptyToNullBehaviour)

case class SuperClass[L <: LA](
    clsName: String,
    tpl: L#TypeName,
    interfaces: List[String],
    params: List[ProtocolParameter[L]],
    discriminators: List[String]
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
        case next@(_ :: _) => work(next, newRequired)
        case Nil => newRequired
      }
    }
    work(List(root), Nil)
  }

  private[this] def fromEnum[L <: LA, F[_]](
      clsName: String,
      swagger: Schema[String]
  )(implicit E: EnumProtocolTerms[L, F], F: FrameworkTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, Either[String, ProtocolElems[L]]] = {
    import E._
    import Sc._

    val toPascalRegexes = List(
      "[\\._-]([a-z])".r, // dotted, snake, or dashed case
      "\\s+([a-zA-Z])".r, // spaces
      "^([a-z])".r // initial letter
    )

    def toPascalCase(s: String): String =
      toPascalRegexes.foldLeft(s)(
        (accum, regex) => regex.replaceAllIn(accum, m => m.group(1).toUpperCase(Locale.US))
      )

    def validProg(enum: List[String], tpe: L#Type): Free[F, EnumDefinition[L]] =
      for {
        elems <- enum.traverse { elem =>
          val termName = toPascalCase(elem)
          for {
            valueTerm <- pureTermName(termName)
            accessor  <- buildAccessor(clsName, termName)
          } yield (elem, valueTerm, accessor)
        }
        pascalValues = elems.map(_._2)
        members <- renderMembers(clsName, elems)
        encoder <- encodeEnum(clsName)
        decoder <- decodeEnum(clsName)

        defn        <- renderClass(clsName, tpe)
        staticDefns <- renderStaticDefns(clsName, members, pascalValues, encoder, decoder)
        classType   <- pureTypeName(clsName)
      } yield EnumDefinition[L](clsName, classType, elems, defn, staticDefns)

    // Default to `string` for untyped enums.
    // Currently, only plain strings are correctly supported anyway, so no big loss.
    val tpeName = Option(swagger.getType).getOrElse("string")
    //fixme why is it objectSchema and not StringSchema

    for {
      enum <- extractEnum(swagger)
      tpe  <- SwaggerUtil.typeName(tpeName, Option(swagger.getFormat()), ScalaType(swagger))
      res  <- enum.traverse(validProg(_, tpe))
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
      hierarchy: ClassParent,
      concreteTypes: List[PropMeta[L]],
      definitions: List[(String, Schema[_])]
  )(implicit F: FrameworkTerms[L, F],
    P: PolyProtocolTerms[L, F],
    M: ModelProtocolTerms[L, F],
    Sc: ScalaTerms[L, F],
    Sw: SwaggerTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import M._
    import P._
    import Sc._

    def child(hierarchy: ClassHierarchy): List[String] =
      hierarchy.children.map(_.name) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.name :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator

    for {
      parents <- hierarchy.model match {
        case c: ComposedSchema => extractParents(c, definitions, concreteTypes)
        case _ => Free.pure[F, List[SuperClass[L]]](Nil)
      }
      props   <- extractProperties(hierarchy.model)
      requiredFields = hierarchy.required ::: hierarchy.children.flatMap(_.required)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse({
        case (name, prop) =>
          val isRequired = requiredFields.contains(name)
          SwaggerUtil
            .propMeta[L, F](prop)
            .flatMap(transformProperty(hierarchy.name, needCamelSnakeConversion, concreteTypes)(name, prop, _, isRequired))
      })
      terms = params.map(_.term)
      definition  <- renderSealedTrait(hierarchy.name, terms, discriminator, parents)
      encoder     <- encodeADT(hierarchy.name, children)
      decoder     <- decodeADT(hierarchy.name, children)
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
      P: PolyProtocolTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, List[SuperClass[L]]] = {
    import M._
    import P._
    import Sc._

    for {
      a <- extractSuperClass(elem, definitions)
      supper <- a.flatTraverse { case (clsName, _extends, interfaces) =>
        val concreteInterfaces = interfaces
          .flatMap(
            x =>
              definitions.collectFirst[Schema[_]] {
                case (cls, y: ComposedSchema) if Option(x.get$ref).exists(_.endsWith(s"/${cls}")) => y
                case (cls, y: Schema[_]) if Option(x.get$ref).exists(_.endsWith(s"/${cls}")) => y
            }
          )
        for {
          _extendsProps <- extractProperties(_extends)
          requiredFields           = getRequiredFieldsRec(_extends) ++ concreteInterfaces.flatMap(getRequiredFieldsRec)
          _withProps    <- concreteInterfaces.traverse(extractProperties)
          props                    = _extendsProps ++ _withProps.flatten
          needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
          params <- props.traverse({
            case (name, prop) =>
              val isRequired = requiredFields.contains(name)
              SwaggerUtil
                .propMeta[L, F](prop)
                .flatMap(transformProperty(clsName, needCamelSnakeConversion, concreteTypes)(name, prop, _, isRequired))
          })
          interfacesCls = interfaces.flatMap(i => Option(i.get$ref).map(_.split("/").last))
          tpe <- parseTypeName(clsName)
        } yield
          tpe
            .map(
              SuperClass[L](
                clsName,
                _,
                interfacesCls,
                params,
                (_extends :: concreteInterfaces).collect {
                  case m: ObjectSchema if Option(m.getDiscriminator).isDefined => m.getDiscriminator.getPropertyName
                }
              )
            )
            .toList
      }

    } yield supper
  }

  private[this] def fromModel[L <: LA, F[_]](clsName: String, model: Schema[_], parents: List[SuperClass[L]], concreteTypes: List[PropMeta[L]])(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Either[String, ProtocolElems[L]]] = {
    import M._
    import Sc._

    for {
      props <- extractProperties(model)
      requiredFields           = getRequiredFieldsRec(model)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse({
        case (name, prop) =>
          val isRequired = requiredFields.contains(name)
          SwaggerUtil.propMeta[L, F](prop).flatMap(transformProperty(clsName, needCamelSnakeConversion, concreteTypes)(name, prop, _, isRequired))
      })
      terms = params.map(_.term)
      defn <- renderDTOClass(clsName, terms, parents)
      deps = params.flatMap(_.dep)
      encoder     <- encodeModel(clsName, needCamelSnakeConversion, params, parents)
      decoder     <- decodeModel(clsName, needCamelSnakeConversion, params, parents)
      staticDefns <- renderDTOStaticDefns(clsName, List.empty, encoder, decoder)
      tpe         <- parseTypeName(clsName)
    } yield
      if (parents.isEmpty && props.isEmpty) Left("Entity isn't model")
      else tpe.toRight("Empty entity name").map(ClassDefinition[L](clsName, _, defn, staticDefns, parents))
  }

  def modelTypeAlias[L <: LA, F[_]](clsName: String, abstractModel: Schema[_])(
      implicit
      F: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import F._
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
          raw => SwaggerUtil.typeName[L, F](raw, model.flatMap(f => Option(f.getFormat)), model.flatMap(ScalaType(_)))
        )
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def plainTypeAlias[L <: LA, F[_]](
      clsName: String
  )(implicit F: FrameworkTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import F._
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

  sealed trait ClassHierarchy {
    def name: String
    def model: Schema[_]
    def children: List[ClassChild]
    def required: List[String]
  }
  case class ClassChild(name: String, model: Schema[_], children: List[ClassChild], required: List[String])                         extends ClassHierarchy
  case class ClassParent(name: String, model: Schema[_], children: List[ClassChild], discriminator: String, required: List[String]) extends ClassHierarchy

  /**
    * returns objects grouped into hierarchies
    */
  def groupHierarchies(definitions: List[(String, Schema[_])]): (List[ClassParent], List[(String, Schema[_])]) = {

    def firstInHierarchy(model: Schema[_]): Option[ObjectSchema] =
      (model match {
        case elem: ComposedSchema =>
          definitions.collectFirst {
            case (clsName, element) if Option(elem.getAllOf).toList.flatMap(_.asScala).exists(r => Option(r.get$ref).exists(_.endsWith(s"/$clsName"))) => element
          }
        case _ => None
      }) match {
        case Some(x: ComposedSchema) => firstInHierarchy(x)
        case Some(x: ObjectSchema)   => Some(x)
        case _                       => None
      }

    def children(cls: String, model: Schema[_]): List[ClassChild] = definitions.collect {
      case (clsName, comp: ComposedSchema)
          if Option(comp.getAllOf)
            .map(_.asScala)
            .getOrElse(List.empty)
            .exists(x => Option(x.get$ref).exists(_.endsWith(s"/$cls"))) =>
        ClassChild(clsName, comp, children(clsName, comp), Option(comp.getRequired()).fold(List.empty[String])(_.asScala.toList))
    }

    def classHierarchy(cls: String, model: Schema[_]): Option[ClassParent] = {
      (model match {
        case c: ComposedSchema => firstInHierarchy(c).flatMap(x => Option(x.getDiscriminator)).flatMap(x => Option(x.getPropertyName).map((_, Option(c.getRequired()).fold(List.empty[String])(_.asScala.toList))))
        case m: Schema[_]      => Option(m.getDiscriminator).flatMap(x => Option(x.getPropertyName).map((_, Option(m.getRequired()).fold(List.empty[String])(_.asScala.toList))))
        case _                 => None
      }).map({ case (a, b) => ClassParent(cls, model, children(cls, model), a, b) })
    }

    definitions.partitionEither({ case (cls, model) =>
      classHierarchy(cls, model).filterNot(_.children.isEmpty).toLeft((cls, model))
    })
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
    val (hierarchies, definitionsWithoutPoly) = groupHierarchies(definitions)

    for {
      concreteTypes <- SwaggerUtil.extractConcreteTypes[L, F](definitions)
      polyADTs      <- hierarchies.traverse(fromPoly(_, concreteTypes, definitions))
      elems <- definitionsWithoutPoly.traverse {
        case (clsName, model) =>
          model match {
            case m: StringSchema =>
              for {
                enum    <- fromEnum(clsName, m)
                model   <- fromModel(clsName, m, List.empty, concreteTypes)
                alias   <- modelTypeAlias(clsName, m)
              } yield enum.orElse(model).getOrElse(alias)

            case comp: ComposedSchema =>
              for {
                parents <- extractParents(comp, definitions, concreteTypes)
                model   <- fromModel(clsName, comp, parents, concreteTypes)
                alias   <- modelTypeAlias(clsName, comp)
              } yield model.getOrElse(alias)

            case arr: ArraySchema =>
              fromArray(clsName, arr, concreteTypes)

            case m: ObjectSchema =>
              for {
                model   <- fromModel(clsName, m, List.empty, concreteTypes)
                alias   <- modelTypeAlias(clsName, m)
              } yield model.getOrElse(alias)

            case x =>
              for {
                tpeName <- getType(x)

                tpe     <- SwaggerUtil.typeName[L, F](tpeName, Option(x.getFormat()), ScalaType(x))
                res     <- typeAlias(clsName, tpe)
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
}
