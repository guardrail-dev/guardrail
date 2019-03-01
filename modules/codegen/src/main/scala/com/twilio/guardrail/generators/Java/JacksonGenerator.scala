package com.twilio.guardrail
package generators
package Java

import _root_.io.swagger.v3.oas.models.media._
import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, Type }
import com.twilio.guardrail.extract.{ Default, ScalaEmptyIsNull, ScalaType }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.{ LA, JavaLanguage }
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.shims._
import com.twilio.guardrail.terms
import java.util.Locale
import scala.collection.JavaConverters._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.{ CompilationUnit, Node, NodeList }
import com.github.javaparser.ast.stmt.{ BlockStmt, ExpressionStmt }
import com.github.javaparser.ast.Modifier.{ PRIVATE, PUBLIC, STATIC }
import com.github.javaparser.ast.expr.{ AssignExpr, BooleanLiteralExpr, DoubleLiteralExpr, Expression, FieldAccessExpr, IntegerLiteralExpr, LiteralExpr, LongLiteralExpr, NameExpr, SimpleName, StringLiteralExpr, ThisExpr }

object JacksonProtocolGenerator {
  import ProtocolGenerator._

  def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[JavaLanguage]])(f: Type => Target[Type]): Option[Target[Type]] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)


  object EnumProtocolTermInterp extends (EnumProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: EnumProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractEnum(swagger) =>
        val enumEntries: Option[List[String]] = swagger match {
          case x: StringSchema =>
            Option[java.util.List[String]](x.getEnum()).map(_.asScala.toList)
          case x =>
            Option[java.util.List[_]](x.getEnum()).map(_.asScala.toList.map(_.toString()))
        }
        Target.pure(Either.fromOption(enumEntries, "Model has no enumerations"))
      case RenderMembers(clsName, elems) =>
        Target.raiseError(s"RenderMembers($clsName, $elems)")
      case EncodeEnum(clsName) =>
        Target.raiseError(s"EncodeEnum($clsName)")
      case DecodeEnum(clsName) =>
        Target.raiseError(s"DecodeEnum($clsName)")
      case RenderClass(clsName, tpe) =>
        Target.raiseError(s"RenderClass($clsName, $tpe)")
      case RenderStaticDefns(clsName, members, accessors, encoder, decoder) =>
        Target.raiseError(s"RenderStaticDefns($clsName, $members, $accessors, $encoder, $decoder)")
      case BuildAccessor(clsName, termName) =>
        Target.raiseError(s"BuildAccessor($clsName, $termName)")
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ModelProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        (swagger match {
          case m: ObjectSchema => Target.pure(Option(m.getProperties))
          case comp: ComposedSchema =>
            Target.pure(Option(comp.getAllOf()).toList.flatMap(_.asScala.toList).lastOption.flatMap(prop => Option(prop.getProperties)))
          case comp: Schema[_] if Option(comp.get$ref).isDefined =>
            Target.error(s"Attempted to extractProperties for a ${comp.getClass()}, unsure what to do here")
          case _ => Target.pure(None)
        }).map(_.map(_.asScala.toList).toList.flatten)

      case TransformProperty(clsName, name, property, meta, needCamelSnakeConversion, concreteTypes, isRequired) =>
        def toCamelCase(s: String): String =
          "[_\\.]([a-z])".r.replaceAllIn(s, m => m.group(1).toUpperCase(Locale.US))

        for {
          _ <- Target.log.debug("definitions", "circe", "modelProtocolTerm")(s"Generated ProtocolParameter(${term}, ${name}, ...)")

          argName = if (needCamelSnakeConversion) toCamelCase(name) else name

          defaultValue <- ((property match {
            case _: MapSchema =>
              Option(safeParseExpression[LiteralExpr]("new java.util.Map<>()").map(x => x: Expression))
            case _: ArraySchema =>
              Option(safeParseExpression[LiteralExpr]("new java.util.List<>()").map(x => x: Expression))
            case p: BooleanSchema =>
              Default(p).extract[Boolean].map(x => Target.pure(new BooleanLiteralExpr(x)))
            case p: NumberSchema if p.getFormat == "double" =>
              Default(p).extract[Double].map(x => Target.pure(new DoubleLiteralExpr(x)))
            case p: NumberSchema if p.getFormat == "float" =>
              Default(p).extract[Float].map(x => Target.pure(new DoubleLiteralExpr(x)))
            case p: IntegerSchema if p.getFormat == "int32" =>
              Default(p).extract[Int].map(x => Target.pure(new IntegerLiteralExpr(x)))
            case p: IntegerSchema if p.getFormat == "int64" =>
              Default(p).extract[Long].map(x => Target.pure(new LongLiteralExpr(x)))
            case p: StringSchema =>
              Default(p).extract[String].map(safeParseExpression[LiteralExpr](_).map(x => x: Expression))
            case _ =>
              None
          }): Option[Target[Expression]]).sequence

          readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
          emptyToNull = (property match {
            case d: DateSchema      => ScalaEmptyIsNull(d)
            case dt: DateTimeSchema => ScalaEmptyIsNull(dt)
            case s: StringSchema    => ScalaEmptyIsNull(s)
            case _                  => None
          }).getOrElse(EmptyIsEmpty)

          tpeClassDep <- meta match {
            case SwaggerUtil.Resolved(declType, classDep, _) =>
              Target.pure((declType, classDep))
            case SwaggerUtil.Deferred(tpeName) =>
              val tpe = concreteTypes.find(_.clsName == tpeName).map(x => Target.pure(x.tpe)).getOrElse {
                println(s"Unable to find definition for ${tpeName}, just inlining")
                safeParseType(tpeName)
              }
              tpe.map((_, Option.empty))
            case SwaggerUtil.DeferredArray(tpeName) =>
              safeParseType(s"java.util.List<${tpeName}>").map((_, Option.empty))
            case SwaggerUtil.DeferredMap(tpeName) =>
              safeParseType(s"java.util.List<${tpeName}>").map((_, Option.empty))
          }
          (tpe, classDep) = tpeClassDep

          _declDefaultPair <- Option(isRequired)
            .filterNot(_ == false)
            .fold[Target[(Type, Option[Expression])]](
              (safeParseType(s"java.util.Optional<${tpe}>"), Option(defaultValue.fold[Target[Expression]](safeParseExpression[Expression](s"java.util.Optional.empty()"))(t => safeParseExpression[Expression](s"java.util.Optional.of($t)"))).sequence).mapN((_, _))
            )(Function.const(Target.pure((tpe, defaultValue))) _)
          (finalDeclType, finalDefaultValue) = _declDefaultPair
          term <- safeParseParameter(s"${finalDeclType} ${argName}") // FIXME: How do we deal with default values? .copy(default = finalDefaultValue)
          dep  = classDep.filterNot(_.value == clsName) // Filter out our own class name
        } yield ProtocolParameter[JavaLanguage](term, name, dep, readOnlyKey, emptyToNull)

      case RenderDTOClass(clsName, selfTerms, parents) =>
        val discriminators = parents.flatMap(_.discriminators)
        val parenOpt       = parents.headOption
        val terms  = (parents.reverse.flatMap(_.params.map(_.term)) ++ selfTerms).filterNot(
          param => discriminators.contains(param.getName().getId())
        )

        val compilationUnit = new CompilationUnit()
        val dtoClass = compilationUnit.addClass(clsName).setPublic(true)

        parenOpt.foreach({ parent =>
          val directParent = new ClassOrInterfaceType(null, new SimpleName(parent.clsName), null, null)
          val otherParents = parent.interfaces.map({ a => new ClassOrInterfaceType(null, new SimpleName(a), null, null) })
          dtoClass.setExtendedTypes(
            new NodeList((directParent +: otherParents): _*)
          )
        })

        terms.foreach( term =>
          dtoClass.addField(term.getType(), term.getNameAsString(), PRIVATE)
        )

        val primaryConstructor = dtoClass.addConstructor(PUBLIC)
        primaryConstructor.setParameters(new NodeList(terms: _*))
        primaryConstructor.setBody(
          new BlockStmt(
            new NodeList(
              terms.map( term =>
                new ExpressionStmt(new AssignExpr(new FieldAccessExpr(new ThisExpr, term.getNameAsString()), new NameExpr(term.getName()), AssignExpr.Operator.ASSIGN))
              ): _*
            )
          )
        )

        Target.raiseError(dtoClass.toString())

      case EncodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.raiseError(s"EncodeModel($clsName, $needCamelSnakeConversion, $selfParams, $parents)")
      case DecodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.raiseError(s"DecodeModel($clsName, $needCamelSnakeConversion, $selfParams, $parents)")
      case RenderDTOStaticDefns(clsName, deps, encoder, decoder) =>
        Target.raiseError(s"RenderDTOStaticDefns($clsName, $deps, $encoder, $decoder)")
    }
  }

  object ArrayProtocolTermInterp extends (ArrayProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ArrayProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractArrayType(arr, concreteTypes) =>
        for {
          result <- arr match {
            case SwaggerUtil.Resolved(tpe, dep, default) => Target.pure(tpe)
            case SwaggerUtil.Deferred(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(Target.pure(_)), s"Unresolved reference ${tpeName}").flatten
            case SwaggerUtil.DeferredArray(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => safeParseType(s"Array<${tpe}>")), s"Unresolved reference ${tpeName}").flatten
            case SwaggerUtil.DeferredMap(tpeName) =>
              Target.fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => safeParseType(s"Array<Map<String, ${tpe}>>")), s"Unresolved reference ${tpeName}").flatten
          }
        } yield result
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ProtocolSupportTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) =>
        Target.raiseError(s"ExtractConcreteTypes($definitions)")
      case ProtocolImports() =>
        Target.raiseError(s"ProtocolImports()")
      case PackageObjectImports() =>
        Target.raiseError(s"PackageObjectImports()")
      case PackageObjectContents() =>
        Target.raiseError(s"PackageObjectContents()")
    }
  }

  object PolyProtocolTermInterp extends (PolyProtocolTerm[JavaLanguage, ?] ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[JavaLanguage, A]): Target[A] = fa match {
      case ExtractSuperClass(swagger, definitions) =>
        Target.raiseError(s"ExtractSuperClass($swagger, $definitions)")
      case RenderADTStaticDefns(clsName, discriminator, encoder, decoder) =>
        Target.raiseError(s"RenderADTStaticDefns($clsName, $discriminator, $encoder, $decoder)")
      case DecodeADT(clsName, children) =>
        Target.raiseError(s"DecodeADT($clsName, $children)")
      case EncodeADT(clsName, children) =>
        Target.raiseError(s"EncodeADT($clsName, $children)")
      case RenderSealedTrait(className, terms, discriminator, parents) =>
        Target.raiseError(s"RenderSealedTrait($className, $terms, $discriminator, $parents)")
    }
  }
}
