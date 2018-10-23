package com.twilio.guardrail

import _root_.io.swagger.models._
import _root_.io.swagger.models.properties.Property
import cats.data.EitherK
import cats.free.Free
import cats.implicits._
import com.twilio.guardrail.extract.ScalaType
import java.util.Locale

import com.twilio.guardrail.generators.GeneratorSettings
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.framework.FrameworkTerms

import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.language.postfixOps
import scala.language.reflectiveCalls
import scala.meta._

case class ProtocolDefinitions(elems: List[StrictProtocolElems],
                               protocolImports: List[Import],
                               packageObjectImports: List[Import],
                               packageObjectContents: List[Stat])

case class ProtocolParameter(term: Term.Param, name: String, dep: Option[Term.Name], readOnlyKey: Option[String], emptyToNullKey: Option[String])

case class SuperClass(
    clsName: String,
    tpl: Type,
    params: List[ProtocolParameter],
    discriminator: Option[String]
)

object ProtocolGenerator {
  private[this] def fromEnum[F[_]](clsName: String, swagger: ModelImpl)(implicit E: EnumProtocolTerms[F],
                                                                        F: FrameworkTerms[F]): Free[F, Either[String, ProtocolElems]] = {
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

    def validProg(enum: List[String], tpe: Type): Free[F, EnumDefinition] = {
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
        values: Defn.Val = q"val values = Vector(..$pascalValues)"
        encoder <- encodeEnum(clsName)
        decoder <- decodeEnum(clsName)

        defn      <- renderClass(clsName, tpe)
        companion <- renderCompanion(clsName, members, accessors, values, encoder, decoder)
      } yield EnumDefinition(clsName, Type.Name(clsName), elems, defn, companion)
    }

    for {
      enum <- extractEnum(swagger)
      tpe  <- extractType(swagger)
      res  <- (enum, tpe).traverseN(validProg)
    } yield res
  }

  def couldBeSnakeCase(s: String): Boolean = s.toLowerCase(Locale.US) == s

  /**
    * Handle polymorphic model
    */
  private[this] def fromPoly[F[_]](
      hierarchy: ClassHierarchy,
      concreteTypes: List[PropMeta],
      definitions: List[(String, Model)]
  )(implicit F: FrameworkTerms[F], P: PolyProtocolTerms[F], M: ModelProtocolTerms[F]): Free[F, ProtocolElems] = {
    import P._
    import M._

    def child(hierarchy: ClassHierarchy): List[String] =
      hierarchy.children.map(_.parentName) ::: hierarchy.children.flatMap(child)
    def parent(hierarchy: ClassHierarchy): List[String] =
      if (hierarchy.children.nonEmpty) hierarchy.parentName :: hierarchy.children.flatMap(parent)
      else Nil

    val children      = child(hierarchy).diff(parent(hierarchy)).distinct
    val discriminator = hierarchy.discriminator.get //fixme unsafe

    for {
      parents <- extractParents(hierarchy.parentModel, definitions, concreteTypes)
      props   <- extractProperties(hierarchy.parentModel)
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse(transformProperty(hierarchy.parentName, needCamelSnakeConversion, concreteTypes) _ tupled)
      terms = params.map(_.term)
      definition <- renderSealedTrait(hierarchy.parentName, terms, discriminator, parents)
      encoder    <- encodeADT(hierarchy.parentName, children)
      decoder    <- decodeADT(hierarchy.parentName, children)
      cmp        <- renderADTCompanion(hierarchy.parentName, discriminator, encoder, decoder)

    } yield {
      ADT(
        name = hierarchy.parentName,
        tpe = Type.Name(hierarchy.parentName),
        trt = definition,
        companion = cmp
      )
    }
  }

  def extractParents[F[_]](elem: Model, definitions: List[(String, Model)], concreteTypes: List[PropMeta])(
      implicit M: ModelProtocolTerms[F],
      F: FrameworkTerms[F]
  ): Free[F, List[SuperClass]] = {
    import scala.collection.JavaConverters._
    import M._

    def allParents(model: Model): List[(String, Model)] =
      (model match {
        case elem: ComposedModel =>
          definitions.collectFirst {
            case (clsName, e) if elem.getInterfaces.asScala.headOption.exists(_.getSimpleRef == clsName) => (clsName, e)
          }
        case _ => None
      }) match {
        case Some(x @ (_, el)) => x :: allParents(el)
        case _                 => Nil
      }

    for {
      a <- Free.pure(allParents(elem))
      supper <- a.traverse { parents =>
        val (clsName, parent) = parents
        for {
          props <- extractProperties(parent)
          needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
          params <- props.traverse(transformProperty(clsName, needCamelSnakeConversion, concreteTypes) _ tupled)
        } yield
          SuperClass(clsName, Type.Name(clsName), params, parent match {
            case m: ModelImpl => Option(m.getDiscriminator)
            case _            => None
          })
      }

    } yield supper
  }

  private[this] def fromModel[F[_]](clsName: String, model: Model, parents: List[SuperClass], concreteTypes: List[PropMeta])(
      implicit M: ModelProtocolTerms[F],
      F: FrameworkTerms[F]
  ): Free[F, Either[String, ProtocolElems]] = {
    import M._
    import F._

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
      else Right(ClassDefinition(clsName, Type.Name(clsName), defn, cmp, parents))
  }

  def modelTypeAlias[F[_]](clsName: String, abstractModel: Model)(
      implicit A: AliasProtocolTerms[F],
      F: FrameworkTerms[F]
  ): Free[F, ProtocolElems] = {
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

  def plainTypeAlias[F[_]](clsName: String)(implicit A: AliasProtocolTerms[F], F: FrameworkTerms[F]): Free[F, ProtocolElems] = {
    import F._
    getGeneratorSettings().flatMap { implicit generatorSettings =>
      typeAlias(clsName, generatorSettings.jsonType)
    }
  }

  def typeAlias[F[_]](clsName: String, tpe: Type)(implicit A: AliasProtocolTerms[F]): Free[F, ProtocolElems] = {
    import A._
    Free.pure(RandomType(clsName, tpe))
  }

  def fromArray[F[_]](clsName: String, arr: ArrayModel, concreteTypes: List[PropMeta])(implicit R: ArrayProtocolTerms[F],
                                                                                       A: AliasProtocolTerms[F]): Free[F, ProtocolElems] = {
    import R._
    for {
      tpe <- extractArrayType(arr, concreteTypes)
      ret <- typeAlias(clsName, tpe)
    } yield ret
  }

  case class ClassHierarchy(parentName: String, parentModel: Model, children: List[ClassHierarchy], discriminator: Option[String] = None)

  /**
    * returns objects grouped into hierarchies
    */
  def groupHierarchies(definitions: List[(String, Model)]): List[ClassHierarchy] = {

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

    def children(cls: String, model: Model): List[ClassHierarchy] = definitions.collect {
      case (clsName, comp: ComposedModel) if comp.getInterfaces.asScala.headOption.exists(_.getSimpleRef == cls) =>
        classHierarchy(clsName, comp)
    }

    def classHierarchy(cls: String, model: Model): ClassHierarchy = ClassHierarchy(
      cls,
      model,
      children(cls, model),
      model match {
        case m: ModelImpl     => Option(m.getDiscriminator)
        case c: ComposedModel => firstInHierarchy(c).map(_.getDiscriminator)
        case _                => None
      }
    )

    definitions.map(classHierarchy _ tupled).filter(_.children.nonEmpty)

  }

  def fromSwagger[F[_]](swagger: Swagger)(
      implicit E: EnumProtocolTerms[F],
      M: ModelProtocolTerms[F],
      A: AliasProtocolTerms[F],
      R: ArrayProtocolTerms[F],
      S: ProtocolSupportTerms[F],
      F: FrameworkTerms[F],
      P: PolyProtocolTerms[F]
  ): Free[F, ProtocolDefinitions] = {
    import S._
    import F._
    import P._

    val definitions = Option(swagger.getDefinitions).toList.flatMap(_.asScala)
    val hierarchies = groupHierarchies(definitions)

    // todo without trait
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

      polyADTElems                           = ProtocolElems.resolve(polyADTs).right.get
      strictElems: List[StrictProtocolElems] = ProtocolElems.resolve(elems).right.get
    } yield ProtocolDefinitions(strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents)
  }
}
