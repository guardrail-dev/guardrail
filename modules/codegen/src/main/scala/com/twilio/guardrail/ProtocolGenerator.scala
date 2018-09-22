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
import com.twilio.guardrail.swagger.{ Escape, SwaggerUtil }
import com.twilio.guardrail.terms.framework.FrameworkTerms

import scala.collection.JavaConverters._
import scala.language.higherKinds
import scala.language.postfixOps
import scala.language.reflectiveCalls
import scala.meta._

case class ProtocolDefinitions(
    elems: List[StrictProtocolElems],
    protocolImports: List[Import],
    packageObjectImports: List[Import],
    packageObjectContents: List[Stat]
)

case class ProtocolParameter(
    term: Term.Param,
    name: String,
    dep: Option[Term.Name],
    readOnlyKey: Option[String],
    emptyToNullKey: Option[String]
)

case class SupperClass(
    clsName: String,
    tpl: Type,
    params: List[ProtocolParameter],
    discriminator: Option[String]
)

object ProtocolGenerator {

  val toPascalRegexes = List(
    "[\\._-]([a-z])".r, // dotted, snake, or dashed case
    "\\s+([a-zA-Z])".r, // spaces
    "^([a-z])".r // initial letter
  )

  def toPascalCase(s: String): String =
    toPascalRegexes.foldLeft(s)(
      (accum, regex) => regex.replaceAllIn(accum, m => m.group(1).toUpperCase(Locale.US))
    )

  private[this] def fromEnum[F[_]](
      clsName: String,
      swagger: ModelImpl
  )(implicit E: EnumProtocolTerms[F], F: FrameworkTerms[F]): Free[F, Either[String, ProtocolElems]] = {
    import E._
    import F._

    def validProg(enum: List[String], tpe: Type): Free[F, EnumDefinition] = {
      val elems = enum.map { elem =>
        val valueTerm = Term.Name(toPascalCase(elem))
        (elem, valueTerm, q"${Term.Name(clsName)}.$valueTerm")
      }
      val pascalValues = elems.map(_._2)
      for {
        members <- renderMembers(clsName, elems)
        accessors = pascalValues.map { pascalValue =>
          q"val ${Pat.Var(pascalValue)}: ${Type.Name(clsName)} = members.$pascalValue"
        }
        values: Defn.Val = q"val values = Vector(..$pascalValues)"
        encoder <- encodeEnum(clsName)
        decoder <- decodeEnum(clsName)

        defn      <- renderClass(clsName, tpe)
        companion <- renderCompanion(clsName, members, accessors, values, encoder, decoder)
      } yield EnumDefinition(clsName, Type.Name(clsName), elems, Escape.escapeTree(defn), Escape.escapeTree(companion))
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

    //fixme: get parameters!!
    //fixme: render companion (probably needed only once per hierarchy -> for the parent)
    //fixme: get Pet's parameters

    val discriminator: String = hierarchy.discriminator

    for {
      parents <- extractParents(hierarchy.parentModel, definitions, concreteTypes)
      props   <- extractProperties(hierarchy.parentModel).map(_.right.get) //fixme unsafe
      needCamelSnakeConversion = props.forall {
        case (k, v) => couldBeSnakeCase(k)
      }
      params <- props.traverse(transformProperty(hierarchy.parentName, needCamelSnakeConversion, concreteTypes) _ tupled)
      terms = params.map(_.term)
      definition <- renderSealedTrait(hierarchy.parentName, terms, discriminator, parents)
      encoder    <- encodeADT(hierarchy.parentName, needCamelSnakeConversion)
      decoder    <- decodeADT(hierarchy.parentName, needCamelSnakeConversion)
      cmp        <- renderDTOCompanion(hierarchy.parentName, List.empty, encoder, decoder)

    } yield {
      ADT(hierarchy.parentName, Type.Name(hierarchy.parentName), definition, cmp)
    }
  }

  def extractParents[F[_]](elem: Model, definitions: List[(String, Model)], concreteTypes: List[PropMeta])(
      implicit M: ModelProtocolTerms[F],
      F: FrameworkTerms[F]
  ): Free[F, List[SupperClass]] = {
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

    def validProg(clsName: String)(props: List[(String, Property)]): Free[F, List[ProtocolParameter]] = {
      val needCamelSnakeConversion = props.forall({
        case (k, v) => couldBeSnakeCase(k)
      })
      for {
        params <- props.traverse(transformProperty(clsName, needCamelSnakeConversion, concreteTypes) _ tupled)
      } yield params
    }

    for {
      a <- Free.pure(allParents(elem))
      supper <- a.traverse { parents =>
        val (clsName, parent) = parents
        for {
          props <- extractProperties(parent)
          proto <- props.traverse(validProg(clsName))
        } yield
          proto.map { a =>
            SupperClass(clsName, Type.Name(clsName), a, parent match {
              case m: ModelImpl => Option(m.getDiscriminator)
              case _            => None
            })
          }
      }

    } yield
      supper.collect {
        case Right(x) => x
      }

  }

  private[this] def fromModel[F[_]](clsName: String, model: Model, parents: List[SupperClass], concreteTypes: List[PropMeta])(
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
    def couldBeSnakeCase(s: String): Boolean = s.toLowerCase(Locale.US) == s

    def validProg: List[(String, Property)] => Free[F, ClassDefinition] = { props =>
      val needCamelSnakeConversion = props.forall({
        case (k, v) => couldBeSnakeCase(k)
      })
      for {
        params <- props.traverse(transformProperty(clsName, needCamelSnakeConversion, concreteTypes) _ tupled)
        terms = params.map(_.term)
        defn <- renderDTOClass(clsName, terms, parents)
        deps = params.flatMap(_.dep)
        encoder <- encodeModel(clsName, needCamelSnakeConversion, params, parents)
        decoder <- decodeModel(clsName, needCamelSnakeConversion, params, parents)
        cmp     <- renderDTOCompanion(clsName, List.empty, encoder, decoder)
      } yield ClassDefinition(clsName, Type.Name(clsName), Escape.escapeTree(defn), Escape.escapeTree(cmp), parents)
    }

    for {
      props <- extractProperties(model)
      res   <- props.traverse(validProg)
    } yield res
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
        .map(_.getType)
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

  def fromArray[F[_]](clsName: String, arr: ArrayModel, concreteTypes: List[PropMeta])(
      implicit R: ArrayProtocolTerms[F],
      A: AliasProtocolTerms[F]
  ): Free[F, ProtocolElems] = {
    import R._
    for {
      tpe <- extractArrayType(arr, concreteTypes)
      ret <- typeAlias(clsName, tpe)
    } yield ret
  }

  case class ClassHierarchy(parentName: String, parentModel: Model, discriminator: String)

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

    definitions
      .collect {
        case (clsName, comp: ComposedModel) if definitions.exists {
              case (_, m: ComposedModel) => m.getInterfaces.asScala.headOption.exists(_.getSimpleRef == clsName)
              case _                     => false
            } =>
          ClassHierarchy(clsName, comp, firstInHierarchy(comp).get.getDiscriminator) //todo unsafe
        case (clsName, model: ModelImpl) if Option(model.getDiscriminator).isDefined =>
          ClassHierarchy(clsName, model, model.getDiscriminator)
      }

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

    val definitions: List[(String, Model)] = Option(swagger.getDefinitions).toList
      .flatMap(_.asScala)

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
              println(s"Warning: $x being treated as Json")
              plainTypeAlias(clsName)
          }
      }
      protoImports      <- protocolImports()
      pkgImports        <- packageObjectImports()
      pkgObjectContents <- packageObjectContents()

      polyADTElems                           = ProtocolElems.resolve(polyADTs).right.get
      strictElems: List[StrictProtocolElems] = ProtocolElems.resolve(elems).right.get
    } yield ProtocolDefinitions(strictElems ++ polyADTElems, protoImports, pkgImports, pkgObjectContents)
  }
}
