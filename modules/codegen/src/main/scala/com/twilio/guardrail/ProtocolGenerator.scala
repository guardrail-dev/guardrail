package com.twilio.guardrail

import _root_.io.swagger.models._
import _root_.io.swagger.models.properties.Property
import cats.data.EitherK
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.extract.ScalaType
import java.util.Locale

import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.languages.LA
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.framework.FrameworkTerms
import com.twilio.guardrail.languages.ScalaLanguage

import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.language.postfixOps
import scala.language.reflectiveCalls
import scala.meta._

case class ProtocolDefinitions[L <: LA](elems: List[StrictProtocolElems[L]],
                                        protocolImports: List[L#Import],
                                        packageObjectImports: List[L#Import],
                                        packageObjectContents: List[L#ValueDefinition])

case class ProtocolParameter[L <: LA](term: L#MethodParameter,
                                      name: String,
                                      dep: Option[L#TermName],
                                      readOnlyKey: Option[String],
                                      emptyToNullKey: Option[String])

case class SuperClass[L <: LA](
    clsName: String,
    tpl: L#Type,
    interfaces: List[String],
    params: List[ProtocolParameter[L]],
    discriminators: List[String]
)

object ProtocolGenerator {
  private[this] def fromEnum[F[_]](clsName: String, swagger: ModelImpl)(implicit E: EnumProtocolTerms[ScalaLanguage, F],
                                                                        F: FrameworkTerms[ScalaLanguage, F]): Free[F, Either[String, ProtocolElems[ScalaLanguage]]] = {
    import E._
    import F._

    val toPascalRegexes = List(
      "[\\._-]([a-z])".r, // dotted, snake, or dashed case
      "\\s+([a-zA-Z])".r, // spaces
      "^([a-z])".r // initial letter
    )

    def toPascalCase(s: String): String =
      toPascalRegexes.foldLeft(s)(
        (accum, regex) => regex.replaceAllIn(accum, m => m.group(1).toUpperCase(Locale.US))
      )

    def validProg(enum: List[String], tpe: Type): Free[F, EnumDefinition[ScalaLanguage]] = {
      val elems = enum.map { elem =>
        val valueTerm = Term.Name(toPascalCase(elem))
        (elem, valueTerm, q"${Term.Name(clsName)}.$valueTerm")
      }
      val pascalValues = elems.map(_._2)
      for {
        members <- renderMembers(clsName, elems)
        accessors = pascalValues
          .map({ pascalValue =>
            q"val ${Pat.Var(pascalValue)}: ${Type.Name(clsName)} = members.${pascalValue}"
          })
          .to[List]
        values = q"val values = Vector(..$pascalValues)"
        encoder <- encodeEnum(clsName)
        decoder <- decodeEnum(clsName)

        defn      <- renderClass(clsName, tpe)
        companion <- renderCompanion(clsName, members, accessors, values, encoder, decoder)
      } yield EnumDefinition[ScalaLanguage](clsName, Type.Name(clsName), elems, defn, companion)
    }

    for {
      enum <- extractEnum(swagger)
      tpe  <- extractType(swagger)
      res  <- (enum, tpe).traverseN(validProg)
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
  private[this] def fromPoly[F[_]](
      hierarchy: ClassParent,
      concreteTypes: List[PropMeta],
      definitions: List[(String, Model)]
  )(implicit F: FrameworkTerms[ScalaLanguage, F], P: PolyProtocolTerms[ScalaLanguage, F], M: ModelProtocolTerms[ScalaLanguage, F]): Free[F, ProtocolElems[ScalaLanguage]] = {
    import P._
    import M._

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
      params <- props.traverse(transformProperty(hierarchy.name, needCamelSnakeConversion, concreteTypes) _ tupled)
      terms = params.map(_.term)
      definition <- renderSealedTrait(hierarchy.name, terms, discriminator, parents)
      encoder    <- encodeADT(hierarchy.name, children)
      decoder    <- decodeADT(hierarchy.name, children)
      cmp        <- renderADTCompanion(hierarchy.name, discriminator, encoder, decoder)

    } yield {
      ADT[ScalaLanguage](
        name = hierarchy.name,
        tpe = Type.Name(hierarchy.name),
        trt = definition,
        companion = cmp
      )
    }
  }

  def extractParents[F[_]](elem: Model, definitions: List[(String, Model)], concreteTypes: List[PropMeta])(
      implicit M: ModelProtocolTerms[ScalaLanguage, F],
      F: FrameworkTerms[ScalaLanguage, F],
      P: PolyProtocolTerms[ScalaLanguage, F]
  ): Free[F, List[SuperClass[ScalaLanguage]]] = {
    import M._
    import P._

    for {
      a <- extractSuperClass(elem, definitions)
      supper <- a.traverse { structure =>
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
          params <- props.traverse(transformProperty(clsName, needCamelSnakeConversion, concreteTypes) _ tupled)
          interfacesCls = interfaces.map(_.getSimpleRef)
        } yield
          SuperClass[ScalaLanguage](
            clsName,
            Type.Name(clsName),
            interfacesCls,
            params,
            (_extends :: concreteInterfaces).collect {
              case m: ModelImpl if Option(m.getDiscriminator).isDefined => m.getDiscriminator
            }
          )
      }

    } yield supper
  }

  private[this] def fromModel[F[_]](clsName: String, model: Model, parents: List[SuperClass[ScalaLanguage]], concreteTypes: List[PropMeta])(
      implicit M: ModelProtocolTerms[ScalaLanguage, F],
      F: FrameworkTerms[ScalaLanguage, F]
  ): Free[F, Either[String, ProtocolElems[ScalaLanguage]]] = {
    import M._
    import F._

    for {
      props <- extractProperties(model)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse(transformProperty(clsName, needCamelSnakeConversion, concreteTypes) _ tupled)
      terms = params.map(_.term)
      defn <- renderDTOClass(clsName, terms, parents)
      deps = params.flatMap(_.dep)
      encoder <- encodeModel(clsName, needCamelSnakeConversion, params, parents)
      decoder <- decodeModel(clsName, needCamelSnakeConversion, params, parents)
      cmp     <- renderDTOCompanion(clsName, List.empty, encoder, decoder)
    } yield
      if (parents.isEmpty && props.isEmpty) Left("Entity isn't model")
      else Right(ClassDefinition[ScalaLanguage](clsName, Type.Name(clsName), defn, cmp, parents))
  }

  def modelTypeAlias[F[_]](clsName: String, abstractModel: Model)(
      implicit A: AliasProtocolTerms[ScalaLanguage, F],
      F: FrameworkTerms[ScalaLanguage, F]
  ): Free[F, ProtocolElems[ScalaLanguage]] = {
    import F._
    val model = abstractModel match {
      case m: ModelImpl => Some(m)
      case m: ComposedModel =>
        m.getAllOf.asScala.toList.get(1).flatMap {
          case m: ModelImpl => Some(m)
          case _            => None
        }
      case _ => None
    }
    getGeneratorSettings().flatMap { implicit generatorSettings =>
      val tpe = model
        .flatMap(model => Option(model.getType))
        .fold[Type](generatorSettings.jsonType)(
          raw => SwaggerUtil.typeName(raw, model.flatMap(f => Option(f.getFormat)), model.flatMap(ScalaType(_)))
        )
      typeAlias(clsName, tpe)
    }
  }

  def plainTypeAlias[F[_]](clsName: String)(implicit A: AliasProtocolTerms[ScalaLanguage, F], F: FrameworkTerms[ScalaLanguage, F]): Free[F, ProtocolElems[ScalaLanguage]] = {
    import F._
    getGeneratorSettings().flatMap { implicit generatorSettings =>
      typeAlias(clsName, generatorSettings.jsonType)
    }
  }

  def typeAlias[F[_]](clsName: String, tpe: Type)(implicit A: AliasProtocolTerms[ScalaLanguage, F]): Free[F, ProtocolElems[ScalaLanguage]] = {
    import A._
    Free.pure(RandomType[ScalaLanguage](clsName, tpe))
  }

  def fromArray[F[_]](clsName: String, arr: ArrayModel, concreteTypes: List[PropMeta])(implicit R: ArrayProtocolTerms[ScalaLanguage, F],
                                                                                                A: AliasProtocolTerms[ScalaLanguage, F]): Free[F, ProtocolElems[ScalaLanguage]] = {
    import R._
    for {
      tpe <- extractArrayType(arr, concreteTypes)
      ret <- typeAlias(clsName, tpe)
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

  def fromSwagger[F[_]](swagger: Swagger)(
      implicit E: EnumProtocolTerms[ScalaLanguage, F],
      M: ModelProtocolTerms[ScalaLanguage, F],
      A: AliasProtocolTerms[ScalaLanguage, F],
      R: ArrayProtocolTerms[ScalaLanguage, F],
      S: ProtocolSupportTerms[ScalaLanguage, F],
      F: FrameworkTerms[ScalaLanguage, F],
      P: PolyProtocolTerms[ScalaLanguage, F]
  ): Free[F, ProtocolDefinitions[ScalaLanguage]] = {
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
      concreteTypes <- extractConcreteTypes(definitions)
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
              plainTypeAlias(clsName)
          }
      }
      protoImports      <- protocolImports
      pkgImports        <- packageObjectImports
      pkgObjectContents <- packageObjectContents

      polyADTElems <- resolveProtocolElems(polyADTs)
      strictElems  <- resolveProtocolElems(elems)
    } yield ProtocolDefinitions[ScalaLanguage](strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents)
  }
}
