package com.twilio.guardrail.generators.Java

import _root_.io.swagger.v3.oas.models.media.{ Discriminator => _, _ }
import cats.Monad
import cats.data.NonEmptyList
import cats.implicits._
import com.github.javaparser.StaticJavaParser
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type, UnknownType }
import com.github.javaparser.ast.Modifier.Keyword.{ FINAL, PRIVATE, PROTECTED, PUBLIC }
import com.github.javaparser.ast.Modifier._
import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr.{ MethodCallExpr, _ }
import com.github.javaparser.ast.stmt._
import com.twilio.guardrail.{
  DataRedacted,
  DataVisible,
  Discriminator,
  EmptyIsEmpty,
  ProtocolParameter,
  RedactionBehaviour,
  RuntimeFailure,
  StaticDefns,
  SuperClass,
  SwaggerUtil,
  Target,
  UserError
}
import com.twilio.guardrail.core.Tracker
import com.twilio.guardrail.core.implicits._
import com.twilio.guardrail.extract.{ DataRedaction, EmptyValueIsNull }
import com.twilio.guardrail.generators.{ JavaGenerator, RawParameterName, RawParameterType }
import com.twilio.guardrail.generators.helpers.JacksonHelpers
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.protocol._
import com.twilio.guardrail.terms.CollectionsLibTerms
import scala.collection.JavaConverters._

object JacksonGenerator {
  private val BUILDER_TYPE        = StaticJavaParser.parseClassOrInterfaceType("Builder")
  private val BIG_INTEGER_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigInteger")
  private val BIG_DECIMAL_FQ_TYPE = StaticJavaParser.parseClassOrInterfaceType("java.math.BigDecimal")

  private case class ParameterTerm(
      propertyName: String,
      parameterName: String,
      fieldType: Type,
      parameterType: Type,
      rawType: RawParameterType,
      defaultValue: Option[Expression],
      dataRedacted: RedactionBehaviour
  )

  // returns a tuple of (requiredTerms, optionalTerms)
  // note that required terms _that have a default value_ are conceptually optional.
  private def sortParams(params: List[ProtocolParameter[JavaLanguage]]): (List[ParameterTerm], List[ParameterTerm]) = {
    def defaultValueToExpression(defaultValue: Option[Node]): Option[Expression] = defaultValue match {
      case Some(expr: Expression) => Some(expr)
      case _                      => None
    }

    params
      .map({ param =>
        val parameterType = if (param.term.getType.isOptional) {
          param.term.getType.containedType.unbox
        } else {
          param.term.getType.unbox
        }
        val defaultValue = defaultValueToExpression(param.defaultValue)

        ParameterTerm(param.name.value, param.term.getNameAsString, param.term.getType.unbox, parameterType, param.rawType, defaultValue, param.dataRedaction)
      })
      .partition(
        pt => !pt.fieldType.isOptional && pt.defaultValue.isEmpty
      )
  }

  private def addParents(cls: ClassOrInterfaceDeclaration, parentOpt: Option[SuperClass[JavaLanguage]]): Unit =
    parentOpt.foreach({ parent =>
      val directParent = StaticJavaParser.parseClassOrInterfaceType(parent.clsName)
      val otherParents = parent.interfaces.map(StaticJavaParser.parseClassOrInterfaceType)
      val _ = cls
        .setExtendedTypes(new NodeList(directParent))
        .setImplementedTypes(otherParents.toNodeList)
    })

  private def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[JavaLanguage]])(f: Type => Target[Type]): Option[Target[Type]] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  // TODO: handle emptyToNull in the return for the getters
  private def addParameterGetter(cls: ClassOrInterfaceDeclaration, param: ParameterTerm): Unit = {
    val _ = cls
      .addMethod(s"get${param.parameterName.unescapeIdentifier.capitalize}", PUBLIC)
      .setType(param.fieldType)
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new FieldAccessExpr(new ThisExpr, param.parameterName))
          )
        )
      )
  }

  private def dtoConstructorBody(superCall: Expression, terms: List[ParameterTerm]): BlockStmt =
    new BlockStmt(
      (
        List[Statement](new ExpressionStmt(superCall)) ++
            terms
              .map(
                term =>
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, term.parameterName),
                      term.fieldType match {
                        case _: PrimitiveType => new NameExpr(term.parameterName)
                        case ft if ft.isOptional =>
                          new ConditionalExpr(
                            new BinaryExpr(new NameExpr(term.parameterName), new NullLiteralExpr, BinaryExpr.Operator.EQUALS),
                            new MethodCallExpr(new NameExpr("Optional"), "empty"),
                            new NameExpr(term.parameterName)
                          )
                        case _ => requireNonNullExpr(term.parameterName)
                      },
                      AssignExpr.Operator.ASSIGN
                    )
                  )
              )
      ).toNodeList
    )

  class EnumProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends EnumProtocolTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractEnum(swagger: Schema[_]) = {
      val enumEntries: Option[List[String]] = swagger match {
        case x: StringSchema =>
          Option[java.util.List[String]](x.getEnum()).map(_.asScala.toList)
        case x =>
          Option[java.util.List[_]](x.getEnum()).map(_.asScala.toList.map(_.toString()))
      }
      Target.pure(Either.fromOption(enumEntries, "Model has no enumerations"))
    }

    def renderMembers(
        clsName: String,
        elems: List[(String, com.github.javaparser.ast.expr.Name, com.github.javaparser.ast.expr.Name)]
    ) =
      Target.pure(None)

    def encodeEnum(clsName: String): Target[Option[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
      Target.pure(None)

    def decodeEnum(clsName: String): Target[Option[BodyDeclaration[_ <: BodyDeclaration[_]]]] =
      Target.pure(None)

    def renderClass(
        clsName: String,
        tpe: com.github.javaparser.ast.`type`.Type,
        elems: List[(String, com.github.javaparser.ast.expr.Name, com.github.javaparser.ast.expr.Name)]
    ) = {
      val enumType = StaticJavaParser.parseType(clsName)

      val enumDefns = elems.map {
        case (value, termName, _) =>
          new EnumConstantDeclaration(
            new NodeList(),
            new SimpleName(termName.getIdentifier),
            new NodeList(new StringLiteralExpr(value)),
            new NodeList()
          )
      }

      val nameField = new FieldDeclaration(
        new NodeList(privateModifier, finalModifier),
        new VariableDeclarator(STRING_TYPE, "name")
      )

      val constructor = new ConstructorDeclaration(new NodeList(privateModifier), clsName)
        .addParameter(new Parameter(new NodeList(finalModifier), STRING_TYPE, new SimpleName("name")))
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
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "name"))
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
                      new MethodCallExpr("value.name.equals", new NameExpr("name")),
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

    def renderStaticDefns(
        clsName: String,
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

    def buildAccessor(clsName: String, termName: String) =
      Target.pure(new Name(s"${clsName}.${termName}"))
  }

  class ModelProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends ModelProtocolTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def renderDTOClass(
        clsName: String,
        supportPackage: List[String],
        selfParams: List[ProtocolParameter[JavaLanguage]],
        parents: List[SuperClass[JavaLanguage]]
    ): Target[TypeDeclaration[_ <: TypeDeclaration[_]]] = {
      val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
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

        discriminators                             = parents.flatMap(_.discriminators)
        discriminatorNames                         = discriminators.map(_.propertyName).toSet
        parentParams                               = parentOpt.toList.flatMap(_.params)
        parentParamNames                           = parentParams.map(_.name)
        (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
        parentTerms                                = parentRequiredTerms ++ parentOptionalTerms

        discriminatorValues <- parentTerms
          .flatMap({ term =>
            discriminators.find(_.propertyName == term.propertyName).map((term, _))
          })
          .traverse({
            case (term, discriminator) =>
              val discriminatorValue = discriminator.mapping
                .collectFirst({ case (value, elem) if elem.name == clsName => value })
                .getOrElse(clsName)

              JacksonHelpers
                .discriminatorExpression[JavaLanguage](
                  discriminator.propertyName,
                  discriminatorValue,
                  term.rawType.tpe,
                  term.rawType.format
                )(
                  v => Target.pure[Node](new ObjectCreationExpr(null, BIG_INTEGER_FQ_TYPE, new NodeList(new StringLiteralExpr(v)))),
                  v => Target.pure[Node](new ObjectCreationExpr(null, BIG_DECIMAL_FQ_TYPE, new NodeList(new StringLiteralExpr(v)))),
                  v =>
                    term.fieldType match {
                      case cls: ClassOrInterfaceType =>
                        // hopefully it's an enum type; nothing else really makes sense here
                        JavaGenerator.JavaInterp.formatEnumName(v).map(ev => new FieldAccessExpr(cls.getNameAsExpression, ev))
                      case tpe =>
                        Target.raiseUserError[Node](s"Unsupported discriminator type '${tpe.asString}' for property '${term.propertyName}'")
                    }
                )(JavaGenerator.JavaInterp)
                .flatMap[Expression]({
                  case expr: Expression => Target.pure(expr)
                  case node =>
                    Target.raiseError(
                      RuntimeFailure(s"BUG: JacksonHelpers.discriminatorExpression() returned a ${node.getClass.getSimpleName} when we need an Expression")
                    )
                })
                .map((term.propertyName, _))
          })
          .map(_.toMap)
      } yield {
        val params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(
                param =>
                  discriminatorNames.contains(param.term.getName.getIdentifier) || parentParamNames.map(_.value).contains(param.term.getName.getIdentifier)
              )
        val (requiredTerms, optionalTerms) = sortParams(params)
        val terms                          = requiredTerms ++ optionalTerms

        val dtoClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier), false, clsName)
        dtoClass.addAnnotation(
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

        addParents(dtoClass, parentOpt)

        def withoutDiscriminators(terms: List[ParameterTerm]): List[ParameterTerm] =
          terms.filterNot(term => discriminatorNames.contains(term.propertyName))

        terms.foreach({
          case ParameterTerm(propertyName, parameterName, fieldType, _, _, _, _) =>
            val field: FieldDeclaration = dtoClass.addField(fieldType, parameterName, PRIVATE, FINAL)
            field.addSingleMemberAnnotation("JsonProperty", new StringLiteralExpr(propertyName))
        })

        val primaryConstructor = dtoClass.addConstructor(PROTECTED)
        primaryConstructor.addMarkerAnnotation("JsonCreator")
        primaryConstructor.setParameters(
          new NodeList(
            withoutDiscriminators(parentTerms ++ terms).map({
              case ParameterTerm(propertyName, parameterName, fieldType, _, _, _, _) =>
                new Parameter(new NodeList(finalModifier), fieldType, new SimpleName(parameterName))
                  .addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(propertyName)))
            }): _*
          )
        )
        val superCall = new MethodCallExpr(
          "super",
          parentTerms.map(term => discriminatorValues.getOrElse(term.propertyName, new NameExpr(term.parameterName))): _*
        )
        primaryConstructor.setBody(dtoConstructorBody(superCall, terms))

        terms.foreach(addParameterGetter(dtoClass, _))

        def parameterGetterCall(term: ParameterTerm, scope: Option[String] = None): MethodCallExpr = {
          val methodName = s"get${term.parameterName.unescapeIdentifier.capitalize}"
          scope.fold(new MethodCallExpr(methodName))(s => new MethodCallExpr(new NameExpr(s), methodName))
        }

        def parameterToStringExpr(term: ParameterTerm, scope: Option[String] = None): Expression = term.dataRedacted match {
          case DataVisible  => parameterGetterCall(term, scope)
          case DataRedacted => new StringLiteralExpr("[redacted]")
        }

        val toStringFieldExprs = NonEmptyList
          .fromList(parentTerms ++ terms)
          .toList
          .flatMap(
            l =>
              (new StringLiteralExpr(s"${l.head.parameterName}="), parameterToStringExpr(l.head)) +:
                  l.tail.map(
                    term =>
                      (
                        new StringLiteralExpr(s", ${term.parameterName}="),
                        parameterToStringExpr(term)
                      )
                  )
          )

        val toStringMethod = dtoClass
          .addMethod("toString", PUBLIC)
          .setType(STRING_TYPE)
          .addMarkerAnnotation("Override")
        toStringMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(
                new BinaryExpr(
                  toStringFieldExprs.foldLeft[Expression](new StringLiteralExpr(s"${clsName}{"))({
                    case (prevExpr, (strExpr, fieldExpr)) =>
                      new BinaryExpr(
                        new BinaryExpr(prevExpr, strExpr, BinaryExpr.Operator.PLUS),
                        fieldExpr,
                        BinaryExpr.Operator.PLUS
                      )
                  }),
                  new StringLiteralExpr("}"),
                  BinaryExpr.Operator.PLUS
                )
              )
            )
          )
        )

        val equalsConditions: List[Expression] = terms.map(
          term =>
            term.fieldType match {
              case _: PrimitiveType =>
                new BinaryExpr(
                  parameterGetterCall(term),
                  parameterGetterCall(term, Some("other")),
                  BinaryExpr.Operator.EQUALS
                )
              case _ =>
                new MethodCallExpr(
                  parameterGetterCall(term),
                  "equals",
                  new NodeList[Expression](parameterGetterCall(term, Some("other")))
                )
            }
        )
        val returnExpr = NonEmptyList
          .fromList(equalsConditions)
          .map(
            _.reduceLeft(
              (prevExpr, condExpr) => new BinaryExpr(prevExpr, condExpr, BinaryExpr.Operator.AND)
            )
          )
          .getOrElse(new BooleanLiteralExpr(true))

        val equalsMethod = dtoClass
          .addMethod("equals", PUBLIC)
          .setType(PrimitiveType.booleanType)
          .addMarkerAnnotation("Override")
          .addParameter(new Parameter(new NodeList(finalModifier), OBJECT_TYPE, new SimpleName("o")))
        equalsMethod.setBody(
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

        val hashCodeMethod = dtoClass
          .addMethod("hashCode", PUBLIC)
          .setType(PrimitiveType.intType)
          .addMarkerAnnotation("Override")
        hashCodeMethod.setBody(
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

        val builderClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, staticModifier), false, "Builder")

        withoutDiscriminators(parentRequiredTerms ++ requiredTerms).foreach({
          case ParameterTerm(_, parameterName, fieldType, _, _, _, _) =>
            builderClass.addField(fieldType, parameterName, PRIVATE)
        })
        withoutDiscriminators(parentOptionalTerms ++ optionalTerms).foreach({
          case ParameterTerm(_, parameterName, fieldType, _, _, defaultValue, _) =>
            val initializer = defaultValue.fold[Expression](
              new MethodCallExpr(new NameExpr("Optional"), "empty")
            )(
              dv =>
                if (fieldType.isOptional) {
                  optionalOfNullableExpr(dv)
                } else {
                  dv
                }
            )
            builderClass.addFieldWithInitializer(fieldType, parameterName, initializer, PRIVATE)
        })

        val builderConstructor = builderClass.addConstructor(PUBLIC)
        builderConstructor.setParameters(
          new NodeList(
            withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map({
              case ParameterTerm(_, parameterName, _, parameterType, _, _, _) =>
                new Parameter(new NodeList(finalModifier), parameterType, new SimpleName(parameterName))
            }): _*
          )
        )
        builderConstructor.setBody(
          new BlockStmt(
            new NodeList(
              withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map({
                case ParameterTerm(_, parameterName, fieldType, _, _, _, _) =>
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
              }): _*
            )
          )
        )

        builderClass
          .addConstructor(PUBLIC)
          .setParameters(new NodeList(new Parameter(new NodeList(finalModifier), dtoClassType, new SimpleName("template"))))
          .setBody(
            new BlockStmt(
              withoutDiscriminators(parentTerms ++ terms)
                .map({
                  case term @ ParameterTerm(_, parameterName, _, _, _, _, _) =>
                    new ExpressionStmt(
                      new AssignExpr(
                        new FieldAccessExpr(new ThisExpr, parameterName),
                        parameterGetterCall(term, Some("template")),
                        AssignExpr.Operator.ASSIGN
                      )
                    ): Statement
                })
                .toNodeList
            )
          )

        // TODO: leave out with${name}() if readOnlyKey?
        withoutDiscriminators(parentTerms ++ terms).foreach({
          case ParameterTerm(_, parameterName, fieldType, parameterType, _, _, _) =>
            val methodName = s"with${parameterName.unescapeIdentifier.capitalize}"

            builderClass
              .addMethod(methodName, PUBLIC)
              .setType(BUILDER_TYPE)
              .addParameter(new Parameter(new NodeList(finalModifier), parameterType, new SimpleName(parameterName)))
              .setBody(
                new BlockStmt(
                  new NodeList(
                    new ExpressionStmt(
                      new AssignExpr(
                        new FieldAccessExpr(new ThisExpr, parameterName),
                        (fieldType, parameterType) match {
                          case (_: PrimitiveType, _) => new NameExpr(parameterName)
                          case (ft, pt) if ft.isOptional && pt.isPrimitiveType =>
                            new MethodCallExpr(new NameExpr("Optional"), "of", new NodeList[Expression](new NameExpr(parameterName)))
                          case (ft, _) if ft.isOptional => optionalOfNullableExpr(new NameExpr(parameterName))
                          case _                        => requireNonNullExpr(parameterName)
                        },
                        AssignExpr.Operator.ASSIGN
                      )
                    ),
                    new ReturnStmt(new ThisExpr)
                  )
                )
              )

            if (!parameterType.isOptional) {
              val newParameterName = s"optional${parameterName.unescapeIdentifier.capitalize}"
              val newParameterType = fieldType match {
                case pt: PrimitiveType   => optionalType(pt.toBoxedType)
                case ft if ft.isOptional => ft
                case ft                  => optionalType(ft)
              }
              builderClass
                .addMethod(methodName, PUBLIC)
                .setType(BUILDER_TYPE)
                .addParameter(new Parameter(new NodeList(finalModifier), newParameterType, new SimpleName(newParameterName)))
                .setBody(
                  new BlockStmt(
                    new NodeList(
                      new ExpressionStmt(
                        if (fieldType.isOptional) {
                          new AssignExpr(
                            new FieldAccessExpr(new ThisExpr, parameterName),
                            requireNonNullExpr(newParameterName),
                            AssignExpr.Operator.ASSIGN
                          )
                        } else {
                          new MethodCallExpr(
                            requireNonNullExpr(newParameterName),
                            "ifPresent",
                            new NodeList[Expression](
                              new LambdaExpr(
                                new NodeList(new Parameter(new UnknownType, parameterName)),
                                new ExpressionStmt(
                                  new AssignExpr(
                                    new FieldAccessExpr(new ThisExpr, parameterName),
                                    new NameExpr(parameterName),
                                    AssignExpr.Operator.ASSIGN
                                  )
                                ),
                                false
                              )
                            )
                          )
                        }
                      ),
                      new ReturnStmt(new ThisExpr)
                    )
                  )
                )
            }
        })

        val builderBuildTerms = withoutDiscriminators(parentTerms ++ terms)
        builderClass
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

        dtoClass.addMember(builderClass)

        dtoClass
      }
    }

    def extractProperties(swagger: Tracker[Schema[_]]) =
      swagger
        .refine({ case m: ObjectSchema => m })(m => Target.pure(m.downField("properties", _.getProperties()).indexedCosequence.value))
        .orRefine({ case c: ComposedSchema => c })(
          comp =>
            Target.pure(
              comp
                .downField("allOf", _.getAllOf())
                .indexedDistribute
                .lastOption
                .toList
                .flatMap(_.downField("properties", _.getProperties).indexedCosequence.value.toList)
            )
        )
        .orRefine({ case x: Schema[_] if Option(x.get$ref()).isDefined => x })(
          comp => Target.raiseUserError(s"Attempted to extractProperties for a ${comp.get.getClass()}, unsure what to do here (${comp.showHistory})")
        )
        .getOrElse(Target.pure(List.empty[(String, Tracker[Schema[_]])]))

    def transformProperty(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        concreteTypes: List[PropMeta[JavaLanguage]]
    )(
        name: String,
        fieldName: String,
        property: Schema[_],
        meta: SwaggerUtil.ResolvedType[JavaLanguage],
        requirement: PropertyRequirement,
        isCustomType: Boolean,
        defaultValue: Option[com.github.javaparser.ast.Node]
    ) =
      Target.log.function("transformProperty") {
        val readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
        val emptyToNull = (property match {
          case d: DateSchema      => EmptyValueIsNull(d)
          case dt: DateTimeSchema => EmptyValueIsNull(dt)
          case s: StringSchema    => EmptyValueIsNull(s)
          case _                  => None
        }).getOrElse(EmptyIsEmpty)
        val dataRedaction = DataRedaction(property).getOrElse(DataVisible)
        for {
          tpeClassDep <- meta match {
            case SwaggerUtil.Resolved(declType, classDep, _, _, _) =>
              Target.pure((declType, classDep))
            case SwaggerUtil.Deferred(tpeName) =>
              val tpe = concreteTypes.find(_.clsName == tpeName).map(x => Target.pure(x.tpe)).getOrElse {
                println(s"Unable to find definition for ${tpeName}, just inlining")
                safeParseType(tpeName)
              }
              tpe.map((_, Option.empty))
            case SwaggerUtil.DeferredArray(tpeName, containerTpe) =>
              for {
                fqListType <- containerTpe.fold(safeParseClassOrInterfaceType("java.util.List")) {
                  case ci: ClassOrInterfaceType => Target.pure(ci)
                  case t                        => Target.raiseUserError(s"Supplied type was not supported: ${t}")
                }
                concreteType = lookupTypeName(tpeName, concreteTypes)(Target.pure)
                innerType <- concreteType.getOrElse(safeParseType(tpeName))
              } yield (fqListType.setTypeArguments(innerType), Option.empty)
            case SwaggerUtil.DeferredMap(tpeName, containerTpe) =>
              for {
                fqMapType <- containerTpe.fold(safeParseClassOrInterfaceType("java.util.Map")) {
                  case ci: ClassOrInterfaceType => Target.pure(ci)
                  case t                        => Target.raiseUserError(s"Supplied type was not supported: ${t}")
                }
                concreteType = lookupTypeName(tpeName, concreteTypes)(Target.pure)
                innerType <- concreteType.getOrElse(safeParseType(tpeName))
              } yield (fqMapType.setTypeArguments(STRING_TYPE, innerType), Option.empty)
          }
          (tpe, classDep) = tpeClassDep

          rawType = RawParameterType(Option(property.getType), Option(property.getFormat))

          expressionDefaultValue <- defaultValue match {
            case Some(e: Expression) => Target.pure(Some(e))
            case Some(_) =>
              Target.log.warning(s"Can't generate default value for class $clsName and property $name.") >> Target.pure(None)
            case None => Target.pure(None)
          }
          (finalDeclType, finalDefaultValue) <- Option(requirement)
            .filter {
              case PropertyRequirement.Required => true
              case _                            => false
            }
            .fold[Target[(Type, Option[Expression])]](
              (
                safeParseType(s"Optional<${tpe}>"),
                Target.pure(
                  Option(
                    expressionDefaultValue
                      .fold(
                        new MethodCallExpr(new NameExpr(s"Optional"), "empty"): Expression
                      )(t => optionalOfExpr(t))
                  )
                )
              ).mapN((_, _))
            )(Function.const(Target.pure((tpe, expressionDefaultValue))) _)
          term <- safeParseParameter(s"final ${finalDeclType} $fieldName")
          dep = classDep.filterNot(_.asString == clsName) // Filter out our own class name
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
          defaultValue
        )
      }

    def encodeModel(
        clsName: String,
        dtoPackage: List[String],
        selfParams: List[ProtocolParameter[JavaLanguage]],
        parents: List[SuperClass[JavaLanguage]] = Nil
    ) =
      Target.pure(None)

    def decodeModel(
        clsName: String,
        dtoPackage: List[String],
        supportPackage: List[String],
        selfParams: List[ProtocolParameter[JavaLanguage]],
        parents: List[SuperClass[JavaLanguage]] = Nil
    ) =
      Target.pure(None)

    def renderDTOStaticDefns(
        clsName: String,
        deps: List[com.github.javaparser.ast.expr.Name],
        encoder: Option[com.github.javaparser.ast.body.VariableDeclarator],
        decoder: Option[com.github.javaparser.ast.body.VariableDeclarator]
    ) =
      Target.pure(StaticDefns(clsName, List.empty, List.empty))
  }

  class ArrayProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends ArrayProtocolTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractArrayType(
        arr: SwaggerUtil.ResolvedType[JavaLanguage],
        concreteTypes: List[PropMeta[JavaLanguage]]
    ): Target[Type] =
      for {
        result <- arr match {
          case SwaggerUtil.Resolved(tpe, dep, default, _, _) => Target.pure(tpe)
          case SwaggerUtil.Deferred(tpeName) =>
            Target.fromOption(lookupTypeName(tpeName, concreteTypes)(Target.pure(_)), UserError(s"Unresolved reference ${tpeName}")).flatten
          case SwaggerUtil.DeferredArray(tpeName, containerTpe) =>
            for {
              tpe <- containerTpe.fold(safeParseClassOrInterfaceType("java.util.List")) {
                case ci: ClassOrInterfaceType => Target.pure(ci)
                case t                        => Target.raiseUserError(s"Supplied type was not supported: ${t}")
              }
              res <- Target
                .fromOption(lookupTypeName(tpeName, concreteTypes)(t => Target.pure(tpe.setTypeArguments(t))), UserError(s"Unresolved reference ${tpeName}"))
                .flatten
            } yield res
          case SwaggerUtil.DeferredMap(tpeName, containerTpe) =>
            for {
              tpe <- containerTpe.fold(safeParseClassOrInterfaceType("java.util.Map")) {
                case ci: ClassOrInterfaceType => Target.pure(ci)
                case t                        => Target.raiseUserError(s"Supplied type was not supported: ${t}")
              }
              res <- Target
                .fromOption(
                  lookupTypeName(tpeName, concreteTypes)(t => safeParseType("java.util.List<${tpe}<String, ${t}>>")),
                  UserError(s"Unresolved reference ${tpeName}")
                )
                .flatten
            } yield res
        }
      } yield result
  }

  class ProtocolSupportTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends ProtocolSupportTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances
    def extractConcreteTypes(definitions: Either[String, List[PropMeta[JavaLanguage]]]) =
      definitions.fold[Target[List[PropMeta[JavaLanguage]]]](Target.raiseUserError, Target.pure)

    def protocolImports() =
      (List(
        "com.fasterxml.jackson.annotation.JsonCreator",
        "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
        "com.fasterxml.jackson.annotation.JsonProperty",
        "com.fasterxml.jackson.annotation.JsonValue",
        "java.util.Optional"
      ).map(safeParseRawImport) ++ List(
            "java.util.Objects.requireNonNull"
          ).map(safeParseRawStaticImport)).sequence

    def staticProtocolImports(pkgName: List[String]) =
      Target.pure(List.empty)

    def generateSupportDefinitions() =
      Target.pure(List.empty)

    def packageObjectImports() =
      Target.pure(List.empty)

    def packageObjectContents() =
      Target.pure(List.empty)

    def implicitsObject() =
      Target.pure(None)
  }

  class PolyProtocolTermInterp(implicit Cl: CollectionsLibTerms[JavaLanguage, Target]) extends PolyProtocolTerms[JavaLanguage, Target] {
    implicit def MonadF: Monad[Target] = Target.targetInstances

    def renderSealedTrait(
        className: String,
        selfParams: List[ProtocolParameter[JavaLanguage]],
        discriminator: Discriminator[JavaLanguage],
        parents: List[SuperClass[JavaLanguage]],
        children: List[String]
    ) = {
      val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
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
      } yield {
        val parentParams                               = parentOpt.toList.flatMap(_.params)
        val parentParamNames                           = parentParams.map(_.name)
        val (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
        val parentTerms                                = parentRequiredTerms ++ parentOptionalTerms
        val params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(
                param => parentParamNames.contains(param.term.getName.getIdentifier)
              )
        val (requiredTerms, optionalTerms) = sortParams(params)
        val terms                          = requiredTerms ++ optionalTerms

        val abstractClass = new ClassOrInterfaceDeclaration(new NodeList(publicModifier, abstractModifier), false, className)
        abstractClass.addAnnotation(
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
        abstractClass.addAnnotation(
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
        abstractClass.addSingleMemberAnnotation(
          "JsonSubTypes",
          new ArrayInitializerExpr(
            new NodeList(
              children.map(
                child =>
                  new NormalAnnotationExpr(
                    new Name("JsonSubTypes.Type"),
                    new NodeList(
                      new MemberValuePair(
                        "name",
                        new StringLiteralExpr(
                          discriminator.mapping
                            .collectFirst({ case (value, elem) if elem.name == child => value })
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

        addParents(abstractClass, parentOpt)

        terms.foreach({ term =>
          val field = abstractClass.addField(term.fieldType, term.parameterName, PRIVATE, FINAL)
          field.addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(term.propertyName)))
        })

        val superCall = new MethodCallExpr("super", parentTerms.map(term => new NameExpr(term.parameterName)): _*)
        abstractClass
          .addConstructor(PROTECTED)
          .setParameters(
            (parentTerms ++ terms)
              .map(term => new Parameter(new NodeList(finalModifier), term.fieldType, new SimpleName(term.parameterName)))
              .toNodeList
          )
          .setBody(dtoConstructorBody(superCall, requiredTerms ++ optionalTerms))

        terms.foreach(addParameterGetter(abstractClass, _))

        abstractClass
      }
    }

    def extractSuperClass(
        swagger: Tracker[ComposedSchema],
        definitions: List[(String, Tracker[Schema[_]])]
    ) = {
      def allParents(model: Tracker[Schema[_]]): List[(String, Tracker[Schema[_]], List[Tracker[Schema[_]]])] =
        model
          .refine({ case c: ComposedSchema => c })(
            _.downField("allOf", _.getAllOf()).indexedDistribute
              .flatMap({ elem =>
                definitions
                  .collectFirst({
                    case (clsName, e) if elem.downField("$ref", _.get$ref()).exists(_.get.endsWith(s"/$clsName")) =>
                      (clsName, e, List.empty) :: allParents(e)
                  })
                  .getOrElse(List.empty)
              })
          )
          .getOrElse(List.empty)

      Target.pure(allParents(swagger))
    }

    def renderADTStaticDefns(
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

    def decodeADT(
        clsName: String,
        discriminator: Discriminator[JavaLanguage],
        children: List[String] = Nil
    ) =
      Target.pure(None)

    def encodeADT(
        clsName: String,
        discriminator: Discriminator[JavaLanguage],
        children: List[String] = Nil
    ) =
      Target.pure(None)
  }
}
