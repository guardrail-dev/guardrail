package com.twilio.guardrail

import _root_.io.swagger.models._
import _root_.io.swagger.models.properties.Property
import cats.data.EitherK
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.extract.ScalaType
import java.util.Locale
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.terms.{ ScalaTerms, SwaggerTerms }

import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.language.postfixOps
import scala.language.reflectiveCalls

case class ProtocolDefinitions[L <: LA](elems: List[StrictProtocolElems[L]],
                                        protocolImports: List[L#Import],
                                        packageObjectImports: List[L#Import],
                                        packageObjectContents: List[L#ValueDefinition])
sealed trait EmptyToNullBehaviour
case object EmptyIsNull extends EmptyToNullBehaviour
case object EmptyIsEmpty extends EmptyToNullBehaviour

case class ProtocolParameter[L <: LA](term: L#MethodParameter,
                                      name: String,
                                      dep: Option[L#TermName],
                                      readOnlyKey: Option[String],
                                      emptyToNullKey: Option[String])

case class SuperClass[L <: LA](
    clsName: String,
    tpl: L#TypeName,
    interfaces: List[String],
    params: List[ProtocolParameter[L]],
    discriminators: List[String]
)

object ProtocolGenerator {
  private[this] def fromEnum[L <: LA, F[_]](
      clsName: String,
      swagger: ModelImpl
  )(implicit E: EnumProtocolTerms[L, F], F: FrameworkTerms[L, F], Sc: ScalaTerms[L, F]): Free[F, Either[String, ProtocolElems[L]]] = {
    import E._
    import F._
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

        defn      <- renderClass(clsName, tpe)
        companion <- renderCompanion(clsName, members, pascalValues, encoder, decoder)
        classType <- pureTypeName(clsName)
      } yield EnumDefinition[L](clsName, classType, elems, defn, companion)

    // Default to `string` for untyped enums.
    // Currently, only plain strings are correctly supported anyway, so no big loss.
    val tpeName = Option(swagger.getType()).getOrElse("string")

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
      definitions: List[(String, Model)]
  )(implicit F: FrameworkTerms[L, F],
    P: PolyProtocolTerms[L, F],
    M: ModelProtocolTerms[L, F],
    Sc: ScalaTerms[L, F],
    Sw: SwaggerTerms[L, F]): Free[F, ProtocolElems[L]] = {
    import P._
    import M._
    import Sc._

    def child(hierarchy: ClassHierarchy): List[String] =
      hierarchy.children.map(_.name) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.name :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator

    for {
      parents <- extractParents(hierarchy.model, definitions, concreteTypes)
      props   <- extractProperties(hierarchy.model)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse({
        case (name, prop) =>
          SwaggerUtil.propMeta[L, F](prop).flatMap(transformProperty(hierarchy.name, needCamelSnakeConversion, concreteTypes)(name, prop, _))
      })
      terms = params.map(_.term)
      definition <- renderSealedTrait(hierarchy.name, terms, discriminator, parents)
      encoder    <- encodeADT(hierarchy.name, children)
      decoder    <- decodeADT(hierarchy.name, children)
      cmp        <- renderADTCompanion(hierarchy.name, discriminator, encoder, decoder)
      tpe        <- pureTypeName(hierarchy.name)
    } yield {
      ADT[L](
        name = hierarchy.name,
        tpe = tpe,
        trt = definition,
        companion = cmp
      )
    }
  }

  def extractParents[L <: LA, F[_]](elem: Model, definitions: List[(String, Model)], concreteTypes: List[PropMeta[L]])(
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
      supper <- a.flatTraverse { structure =>
        val (clsName, _extends, interfaces) = structure
        val concreteInterfaces = interfaces
          .flatMap(
            x =>
              definitions.collectFirst {
                case (cls, y: ModelImpl) if x.getSimpleRef == cls     => y
                case (cls, y: ComposedModel) if x.getSimpleRef == cls => y
            }
          )
        for {
          _extendsProps <- extractProperties(_extends)
          _withProps    <- concreteInterfaces.traverse(extractProperties)
          props                    = _extendsProps ++ _withProps.flatten
          needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
          params <- props.traverse({
            case (name, prop) =>
              SwaggerUtil.propMeta[L, F](prop).flatMap(transformProperty(clsName, needCamelSnakeConversion, concreteTypes)(name, prop, _))
          })
          interfacesCls = interfaces.map(_.getSimpleRef)
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
                  case m: ModelImpl if Option(m.getDiscriminator).isDefined => m.getDiscriminator
                }
              )
            )
            .toList
      }

    } yield supper
  }

  private[this] def fromModel[L <: LA, F[_]](clsName: String, model: Model, parents: List[SuperClass[L]], concreteTypes: List[PropMeta[L]])(
      implicit M: ModelProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, Either[String, ProtocolElems[L]]] = {
    import M._
    import F._
    import Sc._

    for {
      props <- extractProperties(model)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse({
        case (name, prop) =>
          SwaggerUtil.propMeta[L, F](prop).flatMap(transformProperty(clsName, needCamelSnakeConversion, concreteTypes)(name, prop, _))
      })
      terms = params.map(_.term)
      defn <- renderDTOClass(clsName, terms, parents)
      deps = params.flatMap(_.dep)
      encoder <- encodeModel(clsName, needCamelSnakeConversion, params, parents)
      decoder <- decodeModel(clsName, needCamelSnakeConversion, params, parents)
      cmp     <- renderDTOCompanion(clsName, List.empty, encoder, decoder)
      tpe     <- parseTypeName(clsName)
    } yield
      if (parents.isEmpty && props.isEmpty) Left("Entity isn't model")
      else tpe.toRight("Empty entity name").map(ClassDefinition[L](clsName, _, defn, cmp, parents))
  }

  def modelTypeAlias[L <: LA, F[_]](clsName: String, abstractModel: Model)(
      implicit
      F: FrameworkTerms[L, F],
      Sc: ScalaTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import F._
    import Sc._
    val model = abstractModel match {
      case m: ModelImpl => Some(m)
      case m: ComposedModel =>
        m.getAllOf.asScala.toList.get(1).flatMap {
          case m: ModelImpl => Some(m)
          case _            => None
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
    import Sc._
    for {
      tpe <- objectType(None)
      res <- typeAlias[L, F](clsName, tpe)
    } yield res
  }

  def typeAlias[L <: LA, F[_]](clsName: String, tpe: L#Type): Free[F, ProtocolElems[L]] =
    Free.pure(RandomType[L](clsName, tpe))

  def fromArray[L <: LA, F[_]](clsName: String, arr: ArrayModel, concreteTypes: List[PropMeta[L]])(
      implicit R: ArrayProtocolTerms[L, F],
      F: FrameworkTerms[L, F],
      P: ProtocolSupportTerms[L, F],
      Sc: ScalaTerms[L, F],
      Sw: SwaggerTerms[L, F]
  ): Free[F, ProtocolElems[L]] = {
    import P._
    import R._
    for {
      deferredTpe <- SwaggerUtil.modelMetaType(arr)
      tpe         <- extractArrayType(deferredTpe, concreteTypes)
      ret         <- typeAlias[L, F](clsName, tpe)
    } yield ret
  }

  sealed trait ClassHierarchy {
    def name: String
    def model: Model
    def children: List[ClassChild]
  }
  case class ClassChild(name: String, model: Model, children: List[ClassChild])                         extends ClassHierarchy
  case class ClassParent(name: String, model: Model, children: List[ClassChild], discriminator: String) extends ClassHierarchy

  /**
    * returns objects grouped into hierarchies
    */
  def groupHierarchies(definitions: List[(String, Model)]): List[ClassParent] = {

    def firstInHierarchy(model: Model): Option[ModelImpl] =
      (model match {
        case elem: ComposedModel =>
          definitions.collectFirst {
            case (clsName, element) if elem.getInterfaces.asScala.headOption.exists(_.getSimpleRef == clsName) => element
          }
        case _ => None
      }) match {
        case Some(x: ComposedModel) => firstInHierarchy(x)
        case Some(x: ModelImpl)     => Some(x)
        case _                      => None
      }

    def children(cls: String, model: Model): List[ClassChild] = definitions.collect {
      case (clsName, comp: ComposedModel) if comp.getInterfaces.asScala.exists(_.getSimpleRef == cls) =>
        ClassChild(clsName, comp, children(clsName, comp))
    }

    def classHierarchy(cls: String, model: Model): Option[ClassParent] =
      (model match {
        case m: ModelImpl     => Option(m.getDiscriminator)
        case c: ComposedModel => firstInHierarchy(c).map(_.getDiscriminator)
        case _                => None
      }).map(
        ClassParent(
          cls,
          model,
          children(cls, model),
          _
        )
      )

    definitions.map(classHierarchy _ tupled).collect {
      case Some(x) if x.children.nonEmpty => x
    }

  }

  def fromSwagger[L <: LA, F[_]](swagger: Swagger)(
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
    import F._
    import P._

    val definitions = Option(swagger.getDefinitions).toList.flatMap(_.asScala)
    val hierarchies = groupHierarchies(definitions)

    val definitionsWithoutPoly: List[(String, Model)] = definitions.filter { // filter out polymorphic definitions
      case (clsName, _: ComposedModel) if definitions.exists {
            case (_, m: ComposedModel) => m.getInterfaces.asScala.headOption.exists(_.getSimpleRef == clsName)
            case _                     => false
          } =>
        false
      case (_, m: ModelImpl) if Option(m.getDiscriminator).isDefined => false
      case _                                                         => true
    }

    for {
      concreteTypes <- SwaggerUtil.extractConcreteTypes[L, F](definitions)
      polyADTs      <- hierarchies.traverse(fromPoly(_, concreteTypes, definitions))
      elems <- definitionsWithoutPoly.traverse {
        case (clsName, model) =>
          model match {
            case m: ModelImpl =>
              for {
                enum    <- fromEnum(clsName, m)
                parents <- extractParents(m, definitions, concreteTypes)
                model   <- fromModel(clsName, m, parents, concreteTypes)
                alias   <- modelTypeAlias(clsName, m)
              } yield enum.orElse(model).getOrElse(alias)

            case comp: ComposedModel =>
              for {
                parents <- extractParents(comp, definitions, concreteTypes)
                model   <- fromModel(clsName, comp, parents, concreteTypes)
                alias   <- modelTypeAlias(clsName, comp)
              } yield model.getOrElse(alias)

            case arr: ArrayModel =>
              fromArray(clsName, arr, concreteTypes)
            case x =>
              println(s"Warning: ${x} being treated as Json")
              plainTypeAlias[L, F](clsName)
          }
      }
      protoImports      <- protocolImports
      pkgImports        <- packageObjectImports
      pkgObjectContents <- packageObjectContents

      polyADTElems <- ProtocolElems.resolve[L, F](polyADTs)
      strictElems  <- ProtocolElems.resolve[L, F](elems)
    } yield ProtocolDefinitions[L](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents)
  }
}
