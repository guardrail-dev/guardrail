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
        terms = params.map(_.term).to[List]
        defn <- renderDTOClass(clsName, terms)
        deps = params.flatMap(_.dep)
        encoder <- encodeModel(clsName, needCamelSnakeConversion, params)
        decoder <- decodeModel(clsName, needCamelSnakeConversion, params)
        cmp     <- renderDTOCompanion(clsName, List.empty, encoder, decoder)
      } yield ClassDefinition(clsName, Type.Name(clsName), defn, cmp)
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

  def fromArray[F[_]](clsName: String, arr: ArrayModel, concreteTypes: List[PropMeta])(implicit R: ArrayProtocolTerms[F],
                                                                                       A: AliasProtocolTerms[F]): Free[F, ProtocolElems] = {
    import R._
    for {
      tpe <- extractArrayType(arr, concreteTypes)
      ret <- typeAlias(clsName, tpe)
    } yield ret
  }

  def fromSwagger[F[_]](swagger: Swagger)(implicit E: EnumProtocolTerms[F],
                                          M: ModelProtocolTerms[F],
                                          A: AliasProtocolTerms[F],
                                          R: ArrayProtocolTerms[F],
                                          S: ProtocolSupportTerms[F],
                                          F: FrameworkTerms[F]): Free[F, ProtocolDefinitions] = {
    import S._
    import F._

    val definitions = Option(swagger.getDefinitions).toList.flatMap(_.asScala)

    for {
      concreteTypes <- extractConcreteTypes(definitions)
      elems <- definitions.traverse {
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
              println(s"Warning: ${x} being treated as Json")
              plainTypeAlias(clsName)
          }
      }
      protoImports      <- protocolImports
      pkgImports        <- packageObjectImports
      pkgObjectContents <- packageObjectContents
      strictElems = ProtocolElems.resolve(elems).right.get
    } yield ProtocolDefinitions(strictElems, protoImports, pkgImports, pkgObjectContents)
  }
}
