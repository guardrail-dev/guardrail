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
      concreteTypes: List[PropMeta]
  )(implicit F: FrameworkTerms[F], P: PolyProtocolTerms[F], M: ModelProtocolTerms[F]): Free[F, ProtocolElems] = {
    import P._
    import M._

    def compositeSeq(hierarchy: ClassHierarchy): Free[F, List[Either[String, Defn.Class]]] =
      hierarchy.children.traverse { case (childCls, compModel) => composite(hierarchy.parentModel, compModel, childCls) }

    def composite(parent: ModelImpl, model: ComposedModel, className: String): Free[F, Either[String, Defn.Class]] = {
      def validProg(clsName: String)(props: List[(String, Property)]): Free[F, Defn.Class] = {
        val needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
        for {
          params <- props.traverse(transformProperty(clsName, needCamelSnakeConversion, concreteTypes) _ tupled)
          terms = params.map(_.term)
          definition <- renderDTOClass(clsName, terms, Some(hierarchy.parentName))
        } yield {
          Escape.escapeTree(definition)
        }
      }

      for {
        props <- extractChildProperties(parent, model, parent.getDiscriminator)
        res   <- props.traverse(validProg(className))
      } yield res
    }

    for {
      childDefs <- compositeSeq(hierarchy)
      props     <- extractProperties(hierarchy.parentModel).map(_.right.get) //fixme unsafe
      needCamelSnakeConversion = props.forall { case (k, _) => couldBeSnakeCase(k) }
      params <- props.traverse(transformProperty(hierarchy.parentName, needCamelSnakeConversion, concreteTypes) _ tupled)
      terms         = params.map(_.term)
      discriminator = hierarchy.parentModel.getDiscriminator
      definition        <- renderSealedTrait(hierarchy.parentName, terms, discriminator)
      discriminatorStat <- renderDiscriminator(discriminator)
      encoder           <- encodeADT(hierarchy.parentName, needCamelSnakeConversion, params)
      decoder           <- decodeADT(hierarchy.parentName, needCamelSnakeConversion, params)
      cmp               <- renderADTCompanion(hierarchy.parentName, discriminatorStat, encoder, decoder)

    } yield {
      ADT(
        name = hierarchy.parentName,
        tpe = Type.Name(hierarchy.parentName),
        trt = definition,
        children = childDefs.map(_.right.get), //fixme unsafe
        companion = cmp
      )
    }
  }

  private[this] def fromModel[F[_]](clsName: String, model: ModelImpl, concreteTypes: List[PropMeta])(
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
        defn <- renderDTOClass(clsName, terms, None)
        deps = params.flatMap(_.dep)
        encoder <- encodeModel(clsName, needCamelSnakeConversion, params)
        decoder <- decodeModel(clsName, needCamelSnakeConversion, params)
        cmp     <- renderDTOCompanion(clsName, List.empty, encoder, decoder)
      } yield ClassDefinition(clsName, Type.Name(clsName), Escape.escapeTree(defn), Escape.escapeTree(cmp))
    }

    for {
      props <- extractProperties(model)
      res   <- props.traverse(validProg)
    } yield res
  }

  def modelTypeAlias[F[_]](clsName: String, model: ModelImpl)(
      implicit A: AliasProtocolTerms[F],
      F: FrameworkTerms[F]
  ): Free[F, ProtocolElems] = {
    import F._
    getGeneratorSettings().flatMap { implicit generatorSettings =>
      val tpe = Option(model.getType)
        .fold[Type](generatorSettings.jsonType)(raw => SwaggerUtil.typeName(raw, Option(model.getFormat), ScalaType(model)))
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

  case class ClassHierarchy(parentName: String, parentModel: ModelImpl, children: List[(String, ComposedModel)])

  /**
    * returns objects grouped into hierarchies
    */
  def groupHierarchies(definitions: List[(String, Model)]): List[ClassHierarchy] = {

    // parent -> child
    val children: Map[String, List[(String, ComposedModel)]] = definitions
      .map {
        case (clsName, comp: ComposedModel) if comp.getInterfaces.asScala.headOption.map(_.getSimpleRef).isDefined =>
          val parentName = comp.getInterfaces.asScala.headOption.map(_.getSimpleRef).get
          Some((parentName, (clsName, comp)))
        case _ => None
      }
      .collect { case Some(x) => x }
      .groupBy(_._1)
      .mapValues(_.map(_._2))

    val parents: Map[String, ModelImpl] = definitions
      .map { // Fixme here should be List
        case (clsName, impl: ModelImpl) if Option(impl.getDiscriminator).isDefined =>
          Some((clsName, impl))
        case _ => None
      }
      .collect { case Some(x) => x }
      .toMap

    parents
      .map {
        case (parentName, model) =>
          children.get(parentName).map(children => ClassHierarchy(parentName, model, children))
      }
      .collect { case Some(x) => x }
      .toList
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

    val definitionsWithoutPoly: List[(String, Model)] = definitions.filter { // filter out polymorphic definitions
      case (_, _: ComposedModel)                                     => false
      case (_, m: ModelImpl) if Option(m.getDiscriminator).isDefined => false
      case _                                                         => true
    }

    for {
      concreteTypes <- extractConcreteTypes(definitions)
      polyADTs      <- hierarchies.traverse(fromPoly(_, concreteTypes))
      elems <- definitionsWithoutPoly.traverse {
        case (clsName, model) =>
          model match {
            case m: ModelImpl =>
              for {
                enum  <- fromEnum(clsName, m)
                model <- fromModel(clsName, m, concreteTypes)
                alias <- modelTypeAlias(clsName, m)
              } yield enum.orElse(model).getOrElse(alias)

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
