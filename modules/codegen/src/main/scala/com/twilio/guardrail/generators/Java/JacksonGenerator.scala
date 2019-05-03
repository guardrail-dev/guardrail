package com.twilio.guardrail
package generators
package Java

import _root_.io.swagger.v3.oas.models.media._
import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import com.github.javaparser.ast.`type`.{ ClassOrInterfaceType, PrimitiveType, Type }
import com.twilio.guardrail.extract.{ Default, EmptyValueIsNull }
import com.twilio.guardrail.generators.syntax.Java._
import com.twilio.guardrail.generators.syntax.RichString
import com.twilio.guardrail.languages.JavaLanguage
import com.twilio.guardrail.protocol.terms.protocol._
import scala.collection.JavaConverters._
import com.github.javaparser.JavaParser
import com.github.javaparser.ast.{ Node, NodeList }
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.Modifier.{ ABSTRACT, FINAL, PRIVATE, PROTECTED, PUBLIC, STATIC }
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import java.util
import scala.language.existentials
import scala.util.Try

object JacksonGenerator {
  private val BUILDER_TYPE = JavaParser.parseClassOrInterfaceType("Builder")

  private case class ParameterTerm(propertyName: String, parameterName: String, fieldType: Type, parameterType: Type, defaultValue: Option[Expression])

  // returns a tuple of (requiredTerms, optionalTerms)
  // note that required terms _that have a default value_ are conceptually optional.
  private def sortParams(params: List[ProtocolParameter[JavaLanguage]]): (List[ParameterTerm], List[ParameterTerm]) = {
    def defaultValueToExpression(defaultValue: Option[Node]): Option[Expression] = defaultValue match {
      case Some(expr: Expression) => Some(expr)
      case _                      => None
    }

    params
      .map({
        case ProtocolParameter(term, name, _, _, _, selfDefaultValue) =>
          val parameterType = if (term.getType.isOptional) {
            term.getType.containedType.unbox
          } else {
            term.getType.unbox
          }
          val defaultValue = defaultValueToExpression(selfDefaultValue)

          ParameterTerm(name, term.getNameAsString, term.getType.unbox, parameterType, defaultValue)
      })
      .partition(
        pt => !pt.fieldType.isOptional && pt.defaultValue.isEmpty
      )
  }

  private def addParents(cls: ClassOrInterfaceDeclaration, parentOpt: Option[SuperClass[JavaLanguage]]): Unit =
    parentOpt.foreach({ parent =>
      val directParent = JavaParser.parseClassOrInterfaceType(parent.clsName)
      val otherParents = parent.interfaces.map(JavaParser.parseClassOrInterfaceType)
      cls.setExtendedTypes(new NodeList(directParent))
      cls.setImplementedTypes(otherParents.toNodeList)
    })

  private def lookupTypeName(tpeName: String, concreteTypes: List[PropMeta[JavaLanguage]])(f: Type => Target[Type]): Option[Target[Type]] =
    concreteTypes
      .find(_.clsName == tpeName)
      .map(_.tpe)
      .map(f)

  // TODO: handle emptyToNull in the return for the getters
  private def addParameterGetter(cls: ClassOrInterfaceDeclaration, param: ParameterTerm): Unit =
    cls
      .addMethod(s"get${param.parameterName.unescapeIdentifier.capitalize}", PUBLIC)
      .setType(param.fieldType)
      .setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(new FieldAccessExpr(new ThisExpr, param.parameterName))
          )
        )
      )

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
                          new MethodCallExpr(new NameExpr("java.util.Optional"), "empty"),
                          new NameExpr(term.parameterName)
                        )
                      case _ => new MethodCallExpr("requireNonNull", new NameExpr(term.parameterName))
                    },
                    AssignExpr.Operator.ASSIGN
                  )
              )
            )
      ).toNodeList
    )

  private val HASH_MAP_TYPE_DIAMONDED = JavaParser
    .parseClassOrInterfaceType("java.util.HashMap")
    .setTypeArguments(new NodeList[Type])
  private val ARRAY_LIST_TYPE_DIAMONDED = JavaParser
    .parseClassOrInterfaceType("java.util.ArrayList")
    .setTypeArguments(new NodeList[Type])

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
        Target.pure(None)

      case EncodeEnum(clsName) =>
        Target.pure(None)

      case DecodeEnum(clsName) =>
        Target.pure(None)

      case RenderClass(clsName, tpe, elems) =>
        val enumType = JavaParser.parseType(clsName)

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
          util.EnumSet.of(PRIVATE, FINAL),
          new VariableDeclarator(STRING_TYPE, "name")
        )

        val constructor = new ConstructorDeclaration(util.EnumSet.of(PRIVATE), clsName)
        constructor.addParameter(new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("name")))
        constructor.setBody(
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
          util.EnumSet.of(PUBLIC),
          STRING_TYPE,
          "getName"
        )
        getNameMethod.addMarkerAnnotation("JsonValue")
        getNameMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ReturnStmt(new FieldAccessExpr(new ThisExpr, "name"))
            )
          )
        )

        val parseMethod = new MethodDeclaration(
          util.EnumSet.of(PUBLIC, STATIC),
          enumType,
          "parse"
        )
        parseMethod.addMarkerAnnotation("JsonCreator")
        parseMethod.addParameter(new Parameter(util.EnumSet.of(FINAL), STRING_TYPE, new SimpleName("name")))
        parseMethod.setBody(
          new BlockStmt(
            new NodeList(
              new ForEachStmt(
                new VariableDeclarationExpr(new VariableDeclarator(enumType, "value"), FINAL),
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
                  JavaParser.parseClassOrInterfaceType("IllegalArgumentException"),
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

        val staticInitializer = new InitializerDeclaration(
          true,
          new BlockStmt(
            new NodeList(
              new ExpressionStmt(
                new MethodCallExpr(
                  new MethodCallExpr(new NameExpr("Shower"), "getInstance"),
                  "register",
                  new NodeList[Expression](
                    new ClassExpr(JavaParser.parseClassOrInterfaceType(clsName)),
                    new MethodReferenceExpr(new NameExpr(clsName), null, "getName")
                  )
                )
              )
            )
          )
        )

        val enumClass = new EnumDeclaration(
          util.EnumSet.of(PUBLIC),
          new NodeList(),
          new SimpleName(clsName),
          new NodeList(),
          new NodeList(enumDefns: _*),
          new NodeList(
            staticInitializer,
            nameField,
            constructor,
            getNameMethod,
            parseMethod
          )
        )

        Target.pure(enumClass)

      case RenderStaticDefns(clsName, members, accessors, encoder, decoder) =>
        for {
          extraImports <- List(
            "com.fasterxml.jackson.annotation.JsonCreator",
            "com.fasterxml.jackson.annotation.JsonValue"
          ).traverse(safeParseRawImport)
        } yield
          StaticDefns[JavaLanguage](
            className = clsName,
            extraImports = extraImports,
            definitions = List.empty
          )

      case BuildAccessor(clsName, termName) =>
        Target.pure(new Name(s"${clsName}.${termName}"))
    }
  }

  private def renderDTOClass(clsName: String,
                             selfParams: List[ProtocolParameter[JavaLanguage]],
                             parents: List[SuperClass[JavaLanguage]]): Target[TypeDeclaration[_ <: TypeDeclaration[_]]] = {
    val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
    for {
      dtoClassType <- safeParseClassOrInterfaceType(clsName)
      parentOpt <- (parentsWithDiscriminators, parents) match {
        case _ if parentsWithDiscriminators.length > 1 =>
          Target.raiseError[Option[SuperClass[JavaLanguage]]](
            s"${clsName} requires unsupported multiple inheritance due to multiple parents with discriminators (${parentsWithDiscriminators.map(_.clsName).mkString(", ")})"
          )
        case _ if parentsWithDiscriminators.length == 1 => Target.pure(parentsWithDiscriminators.headOption)
        case _ if parents.length == 1                   => Target.pure(parents.headOption)
        case _                                          => Target.pure(None)
      }
    } yield {
      val discriminators                             = parents.flatMap(_.discriminators)
      val parentParams                               = parentOpt.toList.flatMap(_.params)
      val parentParamNames                           = parentParams.map(_.name)
      val (parentRequiredTerms, parentOptionalTerms) = sortParams(parentParams)
      val parentTerms                                = parentRequiredTerms ++ parentOptionalTerms
      val params = parents.filterNot(parent => parentOpt.contains(parent)).flatMap(_.params) ++ selfParams.filterNot(
        param => discriminators.contains(param.term.getName.getIdentifier) || parentParamNames.contains(param.term.getName.getIdentifier)
      )
      val (requiredTerms, optionalTerms) = sortParams(params)
      val terms                          = requiredTerms ++ optionalTerms

      val dtoClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC), false, clsName)
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
        terms.filterNot(term => discriminators.contains(term.propertyName))

      terms.foreach({
        case ParameterTerm(propertyName, parameterName, fieldType, _, _) =>
          val field: FieldDeclaration = dtoClass.addField(fieldType, parameterName, PRIVATE, FINAL)
          field.addSingleMemberAnnotation("JsonProperty", new StringLiteralExpr(propertyName))
      })

      val primaryConstructor = dtoClass.addConstructor(PROTECTED)
      primaryConstructor.addMarkerAnnotation("JsonCreator")
      primaryConstructor.setParameters(
        new NodeList(
          withoutDiscriminators(parentTerms ++ terms).map({
            case ParameterTerm(propertyName, parameterName, fieldType, _, _) =>
              new Parameter(util.EnumSet.of(FINAL), fieldType, new SimpleName(parameterName))
                .addAnnotation(new SingleMemberAnnotationExpr(new Name("JsonProperty"), new StringLiteralExpr(propertyName)))
          }): _*
        )
      )
      val superCall = new MethodCallExpr(
        "super",
        parentTerms.map({ term =>
          if (discriminators.contains(term.propertyName)) {
            new StringLiteralExpr(clsName)
          } else {
            new NameExpr(term.parameterName)
          }
        }): _*
      )
      primaryConstructor.setBody(dtoConstructorBody(superCall, terms))

      terms.foreach(addParameterGetter(dtoClass, _))

      def parameterGetterCall(term: ParameterTerm, scope: Option[String] = None): MethodCallExpr = {
        val methodName = s"get${term.parameterName.unescapeIdentifier.capitalize}"
        scope.fold(new MethodCallExpr(methodName))(s => new MethodCallExpr(new NameExpr(s), methodName))
      }

      val toStringFieldExprs = NonEmptyList
        .fromList(parentTerms ++ terms)
        .toList
        .flatMap(
          l =>
            (new StringLiteralExpr(s"${l.head.parameterName}="), parameterGetterCall(l.head)) +:
              l.tail.map(
              term =>
                (
                  new StringLiteralExpr(s", ${term.parameterName}="),
                  parameterGetterCall(term)
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
        .addParameter(new Parameter(util.EnumSet.of(FINAL), OBJECT_TYPE, new SimpleName("o")))
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
              new VariableDeclarationExpr(new VariableDeclarator(
                                            dtoClassType,
                                            "other",
                                            new CastExpr(dtoClassType, new NameExpr("o"))
                                          ),
                                          FINAL)
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

      val builderClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, STATIC), false, "Builder")

      withoutDiscriminators(parentRequiredTerms ++ requiredTerms).foreach({
        case ParameterTerm(_, parameterName, fieldType, _, _) =>
          builderClass.addField(fieldType, parameterName, PRIVATE, FINAL)
      })
      withoutDiscriminators(parentOptionalTerms ++ optionalTerms).foreach({
        case ParameterTerm(_, parameterName, fieldType, _, defaultValue) =>
          val initializer = defaultValue.fold[Expression](
            new MethodCallExpr(new NameExpr("java.util.Optional"), "empty")
          )(
            dv =>
              if (fieldType.isOptional) {
                new MethodCallExpr(new NameExpr("java.util.Optional"), "of", new NodeList[Expression](dv))
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
            case ParameterTerm(_, parameterName, _, parameterType, _) =>
              new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName))
          }): _*
        )
      )
      builderConstructor.setBody(
        new BlockStmt(
          new NodeList(
            withoutDiscriminators(parentRequiredTerms ++ requiredTerms).map({
              case ParameterTerm(_, parameterName, fieldType, _, _) =>
                new ExpressionStmt(
                  new AssignExpr(
                    new FieldAccessExpr(new ThisExpr, parameterName),
                    fieldType match {
                      case _: PrimitiveType => new NameExpr(parameterName)
                      case _                => new MethodCallExpr("requireNonNull", new NameExpr(parameterName))
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
        .setParameters(new NodeList(new Parameter(util.EnumSet.of(FINAL), dtoClassType, new SimpleName("template"))))
        .setBody(
          new BlockStmt(
            withoutDiscriminators(parentTerms ++ terms)
              .map({
                case term @ ParameterTerm(_, parameterName, _, _, _) =>
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
      withoutDiscriminators(parentOptionalTerms ++ optionalTerms).foreach({
        case ParameterTerm(_, parameterName, fieldType, parameterType, _) =>
          builderClass
            .addMethod(s"with${parameterName.unescapeIdentifier.capitalize}", PUBLIC)
            .setType(BUILDER_TYPE)
            .addParameter(new Parameter(util.EnumSet.of(FINAL), parameterType, new SimpleName(parameterName)))
            .setBody(
              new BlockStmt(
                new NodeList(
                  new ExpressionStmt(
                    new AssignExpr(
                      new FieldAccessExpr(new ThisExpr, parameterName),
                      fieldType match {
                        case _: PrimitiveType    => new NameExpr(parameterName)
                        case ft if ft.isOptional => new MethodCallExpr("java.util.Optional.of", new NameExpr(parameterName))
                        case _                   => new MethodCallExpr("requireNonNull", new NameExpr(parameterName))
                      },
                      AssignExpr.Operator.ASSIGN
                    )
                  ),
                  new ReturnStmt(new ThisExpr)
                )
              )
            )

          if (fieldType.isOptional && !parameterType.isOptional) {
            builderClass
              .addMethod(s"with${parameterName.unescapeIdentifier.capitalize}", PUBLIC)
              .setType(BUILDER_TYPE)
              .addParameter(new Parameter(util.EnumSet.of(FINAL), fieldType, new SimpleName(parameterName)))
              .setBody(
                new BlockStmt(
                  new NodeList(
                    new ExpressionStmt(
                      new AssignExpr(
                        new FieldAccessExpr(new ThisExpr, parameterName),
                        new MethodCallExpr("requireNonNull", new NameExpr(parameterName)),
                        AssignExpr.Operator.ASSIGN
                      )
                    ),
                    new ReturnStmt(new ThisExpr)
                  )
                )
              )
          }
      })

      val buildMethod = builderClass.addMethod("build", PUBLIC)
      buildMethod.setType(clsName)
      buildMethod.setBody(
        new BlockStmt(
          new NodeList(
            new ReturnStmt(
              new ObjectCreationExpr(
                null,
                JavaParser.parseClassOrInterfaceType(clsName),
                new NodeList(
                  withoutDiscriminators(parentTerms ++ terms).map(param => new FieldAccessExpr(new ThisExpr, param.parameterName)): _*
                )
              )
            )
          )
        )
      )

      dtoClass.addMember(builderClass)

      dtoClass
    }
  }

  object ModelProtocolTermInterp extends (ModelProtocolTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ModelProtocolTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractProperties(swagger) =>
        (swagger match {
          case m: ObjectSchema => Target.pure(Option(m.getProperties))
          case comp: ComposedSchema =>
            Target.pure(Option(comp.getAllOf).flatMap(_.asScala.toList.lastOption).flatMap(prop => Option(prop.getProperties)))
          case comp: Schema[_] if Option(comp.get$ref).isDefined =>
            Target.raiseError(s"Attempted to extractProperties for a ${comp.getClass()}, unsure what to do here")
          case _ => Target.pure(None)
        }).map(_.map(_.asScala.toList).toList.flatten)

      case TransformProperty(clsName, name, property, meta, needCamelSnakeConversion, concreteTypes, isRequired) =>
        Target.log.function("transformProperty") {
          for {
            defaultValue <- property match {
              case _: MapSchema =>
                Target.pure(Option(new ObjectCreationExpr(null, HASH_MAP_TYPE_DIAMONDED, new NodeList())).map(x => x: Expression))
              case _: ArraySchema =>
                Target.pure(Option(new ObjectCreationExpr(null, ARRAY_LIST_TYPE_DIAMONDED, new NodeList())).map(x => x: Expression))
              case p: BooleanSchema =>
                Default(p).extract[Boolean].traverse(x => Target.pure(new BooleanLiteralExpr(x)))
              case p: NumberSchema if p.getFormat == "double" =>
                Default(p).extract[Double].traverse(x => Target.pure(new DoubleLiteralExpr(x)))
              case p: NumberSchema if p.getFormat == "float" =>
                Default(p).extract[Float].traverse(x => Target.pure(new DoubleLiteralExpr(x)))
              case p: IntegerSchema if p.getFormat == "int32" =>
                Default(p).extract[Int].traverse(x => Target.pure(new IntegerLiteralExpr(x)))
              case p: IntegerSchema if p.getFormat == "int64" =>
                Default(p).extract[Long].traverse(x => Target.pure(new LongLiteralExpr(x)))
              case p: StringSchema =>
                Default(p)
                  .extract[String]
                  .traverse(x => Target.fromOption(Try(new StringLiteralExpr(x)).toOption, s"Default string literal for '${p.getTitle}' is null"))
              case _ =>
                Target.pure(None)
            }

            readOnlyKey = Option(name).filter(_ => Option(property.getReadOnly).contains(true))
            emptyToNull = (property match {
              case d: DateSchema      => EmptyValueIsNull(d)
              case dt: DateTimeSchema => EmptyValueIsNull(dt)
              case s: StringSchema    => EmptyValueIsNull(s)
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

            argName = if (needCamelSnakeConversion) name.toCamelCase else name
            _declDefaultPair <- Option(isRequired)
              .filterNot(_ == false)
              .fold[Target[(Type, Option[Expression])]](
                (
                  safeParseType(s"java.util.Optional<${tpe}>"),
                  Target.pure(
                    Option(
                      defaultValue
                        .fold(
                          new MethodCallExpr(new NameExpr(s"java.util.Optional"), "empty")
                        )(t => new MethodCallExpr("java.util.Optional.of", t))
                    )
                  )
                ).mapN((_, _))
              )(Function.const(Target.pure((tpe, defaultValue))) _)
            (finalDeclType, finalDefaultValue) = _declDefaultPair
            term <- safeParseParameter(s"final ${finalDeclType} ${argName.escapeIdentifier}")
            dep = classDep.filterNot(_.value == clsName) // Filter out our own class name
          } yield ProtocolParameter[JavaLanguage](term, name, dep, readOnlyKey, emptyToNull, defaultValue)
        }

      case RenderDTOClass(clsName, selfParams, parents) =>
        renderDTOClass(clsName, selfParams, parents)

      case EncodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.pure(None)

      case DecodeModel(clsName, needCamelSnakeConversion, selfParams, parents) =>
        Target.pure(None)

      case RenderDTOStaticDefns(clsName, deps, encoder, decoder) =>
        Target.pure(StaticDefns(clsName, List.empty, List.empty))
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
              Target
                .fromOption(lookupTypeName(tpeName, concreteTypes)(tpe => safeParseType(s"Array<Map<String, ${tpe}>>")), s"Unresolved reference ${tpeName}")
                .flatten
          }
        } yield result
    }
  }

  object ProtocolSupportTermInterp extends (ProtocolSupportTerm[JavaLanguage, ?] ~> Target) {
    def apply[T](term: ProtocolSupportTerm[JavaLanguage, T]): Target[T] = term match {
      case ExtractConcreteTypes(definitions) =>
        definitions.fold[Target[List[PropMeta[JavaLanguage]]]](Target.raiseError, Target.pure)

      case ProtocolImports() =>
        (List(
          "com.fasterxml.jackson.annotation.JsonCreator",
          "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
          "com.fasterxml.jackson.annotation.JsonProperty"
        ).map(safeParseRawImport) ++ List(
          "java.util.Objects.requireNonNull"
        ).map(safeParseRawStaticImport)).sequence

      case PackageObjectImports() =>
        Target.pure(List.empty)

      case PackageObjectContents() =>
        Target.pure(List.empty)
    }
  }

  private def renderSealedTrait(className: String,
                                selfParams: List[ProtocolParameter[JavaLanguage]],
                                discriminator: String,
                                parents: List[SuperClass[JavaLanguage]],
                                children: List[String]): Target[ClassOrInterfaceDeclaration] = {
    val parentsWithDiscriminators = parents.collect({ case p if p.discriminators.nonEmpty => p })
    for {
      parentOpt <- (parentsWithDiscriminators, parents) match {
        case _ if parentsWithDiscriminators.length > 1 =>
          Target.raiseError[Option[SuperClass[JavaLanguage]]](
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

      val abstractClass = new ClassOrInterfaceDeclaration(util.EnumSet.of(PUBLIC, ABSTRACT), false, className)
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
              new StringLiteralExpr(discriminator)
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
                    new MemberValuePair("name", new StringLiteralExpr(child)),
                    new MemberValuePair("value", new ClassExpr(JavaParser.parseType(child)))
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
            .map(term => new Parameter(util.EnumSet.of(FINAL), term.fieldType, new SimpleName(term.parameterName)))
            .toNodeList
        )
        .setBody(dtoConstructorBody(superCall, requiredTerms ++ optionalTerms))

      terms.foreach(addParameterGetter(abstractClass, _))

      abstractClass
    }
  }

  object PolyProtocolTermInterp extends (PolyProtocolTerm[JavaLanguage, ?] ~> Target) {
    override def apply[A](fa: PolyProtocolTerm[JavaLanguage, A]): Target[A] = fa match {
      case ExtractSuperClass(swagger, definitions) =>
        def allParents(model: Schema[_]): List[(String, Schema[_], List[Schema[_]])] =
          model match {
            case schema: ComposedSchema =>
              Option(schema.getAllOf)
                .map(_.asScala.toList)
                .getOrElse(List.empty)
                .flatMap({ elem =>
                  definitions
                    .collectFirst({
                      case (clsName, e) if Option(elem.get$ref).exists(_.endsWith(s"/$clsName")) =>
                        (clsName, e, List.empty) :: allParents(e)
                    })
                    .getOrElse(List.empty)
                })
            case _ => List.empty
          }

        Target.pure(allParents(swagger))

      case RenderADTStaticDefns(clsName, discriminator, encoder, decoder) =>
        for {
          extraImports <- List(
            "com.fasterxml.jackson.annotation.JsonIgnoreProperties",
            "com.fasterxml.jackson.annotation.JsonSubTypes",
            "com.fasterxml.jackson.annotation.JsonTypeInfo"
          ).traverse(safeParseRawImport)
        } yield
          StaticDefns[JavaLanguage](
            clsName,
            extraImports,
            List.empty
          )

      case DecodeADT(clsName, children) =>
        Target.pure(None)

      case EncodeADT(clsName, children) =>
        Target.pure(None)

      case RenderSealedTrait(className, selfParams, discriminator, parents, children) =>
        renderSealedTrait(className, selfParams, discriminator, parents, children)
    }
  }
}
